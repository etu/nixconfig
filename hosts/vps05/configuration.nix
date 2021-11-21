# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

in
{
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./persistence.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.03";

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "vps05";

  # Set up ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "8425e349";
  services.zfs.autoScrub.enable = true;

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;

  # Enable snapshotting for some filesystems
  services.sanoid.datasets."zroot/persistent".use_template = [ "default" ];

  # Install mosh
  programs.mosh.enable = true;

  # Install fish
  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Set up Letsencrypt
  security.acme.email = config.my.user.email;
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
  services.gitea.cookieSecure = true;
  services.gitea.domain = "git.elis.nu";
  services.gitea.rootUrl = "https://git.elis.nu/";
  services.gitea.database.type = "postgres";
  services.gitea.database.passwordFile = "/persistent/var/lib/gitea-db-pass";
  services.gitea.disableRegistration = true;

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

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Set up users accounts:
  users.mutableUsers = false;

  # Enable a user to do deployments with
  my.deploy-user.enable = true;

  users.users.root.openssh.authorizedKeys.keys = keys.etu.computers ++ keys.etu.syncoid;
}
