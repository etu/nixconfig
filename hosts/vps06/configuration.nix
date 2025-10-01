# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  flake,
  ...
}:
{
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Include static network settings.
    ./networking.nix

    # Import matrix settings
    ./services/forgejo.nix
    ./services/matrix.nix
    ./services/misc.nix
    ./services/postgres.nix

    # Import my default modules
    flake.nixosModules.default
  ];

  # Set hostname
  networking.hostName = "vps06";

  # Settings needed for ZFS
  networking.hostId = "77a4b2cc";

  # Enable weekly garbage-collection and daily store optimization.
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
  nix.optimise.automatic = true;
  nix.optimise.dates = [ "daily" ];

  # My module settings
  etu = {
    stateVersion = "24.05";

    base.emacs.enable = lib.mkForce false;

    user.extraRootAuthorizedKeys =
      # Allow home server to pull backups
      config.etu.data.pubkeys.etu.syncoid.server-main-elis
      ++
        # Allow github to deploy system
        config.etu.data.pubkeys.etu.github-actions;

    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = [ "data" ];
    };
    services.netdata.enable = true;

    # Allow beszel to monitor this system
    services.beszel-agent.enable = true;
    services.beszel-agent.extraFilesystems = [
      "/boot"
      "/data"
      "/data/local"
      "/nix"
    ];
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Set up Letsencrypt
  security.acme.defaults.email = config.etu.user.email;
  security.acme.acceptTerms = true;

  etu.base.zfs.system.directories = [
    # Persistence of certificates for nginx
    "/var/lib/acme"
  ];

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.nginx.enable = true;
}
