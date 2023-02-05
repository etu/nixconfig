# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  myData,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Include static network settings.
    ./networking.nix

    # Import matrix settings
    ./services/gitea.nix
    ./services/hockeypuck.nix
    ./services/matrix.nix
    ./services/misc.nix
    ./services/postgres.nix
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
  nix.optimise.dates = ["daily"];

  # My module settings
  etu = {
    stateVersion = "22.11";

    base.emacs.enable = lib.mkForce false;
    base.telegraf.enable = true;
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.syncoid.server-main-elis;
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
    };
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Install mosh
  programs.mosh.enable = true;

  # Set up Letsencrypt
  security.acme.defaults.email = config.etu.user.email;
  security.acme.acceptTerms = true;

  etu.base.zfs.system.directories = [
    # Persistence of certificates for nginx
    "/var/lib/acme"
  ];

  networking.firewall.allowedTCPPorts = [80 443];

  services.nginx.enable = true;
}
