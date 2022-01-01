# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

  # Load nivSources
  nivSources = import ../../nix/sources.nix;
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
  services.sanoid.datasets."zroot/persistent".use_template = [ "persistent" ];

  # Install mosh
  programs.mosh.enable = true;

  # Install fish
  programs.fish.enable = true;

  # List services that you want to enable:

  # Override identity paths for agenix since the openssh default paths
  # relies on a symlink being created in /etc/ssh to point at the
  # right path to make it to work as it would be in the right place.
  age.identityPaths = [
    "/persistent/etc/ssh/ssh_host_ed25519_key"
    "/persistent/etc/ssh/ssh_host_rsa_key"
  ];

  # Include agenix encripted secrets for cloudflare origin server
  # certificates so we can have an encrypted connection from
  # cloudflare to this server.
  age.secrets.xn--hlsobrev-0za-se-pem = {
    file = ../../secrets/certs/xn--hlsobrev-0za.se.pem.age;
    owner = "nginx";
  };
  age.secrets.xn--hlsobrev-0za-se-key = {
    file = ../../secrets/certs/xn--hlsobrev-0za.se.key.age;
    owner = "nginx";
  };
  age.secrets.halsobrev-se-pem = {
    file = ../../secrets/certs/halsobrev.se.pem.age;
    owner = "nginx";
  };
  age.secrets.halsobrev-se-key = {
    file = ../../secrets/certs/halsobrev.se.key.age;
    owner = "nginx";
  };

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
    # Serve main domain.
    "xn--hlsobrev-0za.se" = {
      sslCertificate = config.age.secrets.xn--hlsobrev-0za-se-pem.path;
      sslCertificateKey = config.age.secrets.xn--hlsobrev-0za-se-key.path;
      addSSL = true;
      locations."/".root = pkgs.callPackage "${nivSources.halsobrev}/default.nix" {};
    };
    # Redirect from www to main domain.
    "www.xn--hlsobrev-0za.se" = {
      addSSL = true;
      sslCertificate = config.age.secrets.xn--hlsobrev-0za-se-pem.path;
      sslCertificateKey = config.age.secrets.xn--hlsobrev-0za-se-key.path;
      globalRedirect = "xn--hlsobrev-0za.se";
    };
    # Redirect from alt domain to main domain.
    "halsobrev.se" = {
      addSSL = true;
      sslCertificate = config.age.secrets.halsobrev-se-pem.path;
      sslCertificateKey = config.age.secrets.halsobrev-se-key.path;
      serverAliases = [ "www.halsobrev.se" ];
      globalRedirect = "xn--hlsobrev-0za.se";
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
