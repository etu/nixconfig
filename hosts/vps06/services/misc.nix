{ pkgs, ... }:

let
  # Import my ssh public keys
  sources = import ../../../nix/sources.nix;
  via-elis-nu = pkgs.callPackage "${sources.via-elis-nu}/default.nix" { };

in
{
  # Enable the ip-failar-nu service
  services.ip-failar-nu.enable = true;

  # Disable validation because it breaks when doing proxyPass to a
  # remote host like with elis.nu proxing below for hosts sa.0b.se and
  # sa4b.se that are just... replicas of my website.
  services.nginx.validateConfig = false;

  services.nginx.virtualHosts = {
    "ip.failar.nu" = {
      addSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:8123/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };
    "sa.0b.se" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "https://elis.nu/";
    };
    "sa4b.se" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "https://elis.nu/";
    };
    "keys.ix.ufs.se" = {
      forceSSL = true;
      enableACME = true;
      globalRedirect = "keys.proxxi.org";
    };
    "via.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".root = via-elis-nu;
    };
  };
}
