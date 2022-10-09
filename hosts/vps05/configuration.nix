# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:
let
  # Import my ssh public keys
  keys = (import ../../data.nix).pubkeys;
in
{
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Include static network settings.
    ./networking.nix

    # Import local modules
    ../../modules
  ];

  # Set hostname
  networking.hostName = "vps05";

  # Settings needed for ZFS
  networking.hostId = "8425e349";

  # My module settings
  etu = {
    stateVersion = "22.11";

    base.emacs.enable = lib.mkForce false;
    user.extraRootAuthorizedKeys = keys.etu.syncoid.fenchurch;
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = [ "data" ];
    };
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Install mosh
  programs.mosh.enable = true;

  # List services that you want to enable:

  # Set up Letsencrypt
  security.acme.defaults.email = config.etu.user.email;
  security.acme.acceptTerms = true;

  # Set up NGiNX
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "git.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:3000/";
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
    "ip.failar.nu" = {
      addSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:8123/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };
    "keys.ix.ufs.se" = {
      forceSSL = true;
      enableACME = true;
      globalRedirect = "keys.proxxi.org";
    };
    "keys.proxxi.org" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:11371/";
    };
  };

  # Open Firewall for HTTP, HTTPS and hkp (keyserver)
  networking.firewall.allowedTCPPorts = [ 80 443 11371 ];

  # Gitea
  services.gitea.enable = true;
  services.gitea.appName = "Elis Git Service";
  services.gitea.domain = "git.elis.nu";
  services.gitea.rootUrl = "https://git.elis.nu/";
  services.gitea.database.type = "postgres";
  services.gitea.database.passwordFile = "${config.etu.dataPrefix}/var/lib/gitea-db-pass";
  services.gitea.settings.session.COOKIE_SECURE = true;
  services.gitea.settings.service.DISABLE_REGISTRATION = true;

  # Postgres
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql_11;
  services.postgresql.ensureDatabases = [
    "hockeypuck"
  ];
  services.postgresql.ensureUsers = [{
    name = "hockeypuck";
    ensurePermissions."DATABASE hockeypuck" = "ALL PRIVILEGES";
  }];

  # Enable the ip-failar-nu service
  services.ip-failar-nu.enable = true;

  # Enable hockeypuck keyserver
  services.hockeypuck.enable = true;
}
