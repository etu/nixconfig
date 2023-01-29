# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, myData, ... }:

{
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Some networking wait tooks for certain services
    ./network-wait-hook.nix

    # Import local services that are host specific
    ./services/cfdyndns.nix
    ./services/guest-users.nix
    ./services/hass.nix
    ./services/home-nginx.nix
    ./services/monitoring.nix
    ./services/svtplay.nix
    ./services/usenet.nix
  ];

  # Set hostname
  networking.hostName = "server-main-elis";

  # Enable networking.
  networking.useDHCP = true;

  # Settings needed for ZFS
  networking.hostId = "23916528";

  # Enable weekly garbage-collection and daily store optimization.
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
  nix.optimise.automatic = true;
  nix.optimise.dates = [ "daily" ];

  # My module settings
  etu = {
    stateVersion = "22.11";

    base.emacs.package = "nox";
    development.git.enable = true;
    user.enable = true;
    user.extraGroups = [ "libvirtd" ];
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.syncoid.workstations;
    services.freshrss.enable = true;
    services.jellyfin.enable = true;
    services.syncthing.enable = true;
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = [ "data" ];
      "zroot/safe/home".use_template = [ "home" ];

      # Enable cleanup for synced backups
      "zroot/backups/current/laptop-private-elis/data" = { use_template = [ "data" ]; autosnap = false; };
      "zroot/backups/current/laptop-private-elis/home" = { use_template = [ "home" ]; autosnap = false; };
      "zroot/backups/current/laptop-work-elis/data" = { use_template = [ "data" ]; autosnap = false; };
      "zroot/backups/current/laptop-work-elis/home" = { use_template = [ "home" ]; autosnap = false; };
      "zroot/backups/current/vps04/data" = { use_template = [ "data" ]; autosnap = false; };
      "zroot/backups/current/vps04/home" = { use_template = [ "home" ]; autosnap = false; };
      "zroot/backups/current/vps06/data" = { use_template = [ "data" ]; autosnap = false; };

      # Enable snapshotting for bulk storage
      "zstorage/files".use_template = [ "storage" ];
      "zstorage/files/audio".use_template = [ "storage" ];
      "zstorage/files/ebooks".use_template = [ "storage" ];
      "zstorage/files/software".use_template = [ "storage" ];
      "zstorage/files/upload".use_template = [ "storage" ];
      "zstorage/files/video".use_template = [ "storage" ];
    };
    base.syncoid.enable = true;
    # Enable syncing of some filesystems
    base.syncoid.commands = {
      "root@vps04.elis.nu:zroot/safe/data".target = "zroot/backups/current/vps04/data";
      "root@vps04.elis.nu:zroot/safe/home".target = "zroot/backups/current/vps04/home";
      "root@vps06.elis.nu:zroot/safe/data".target = "zroot/backups/current/vps06/data";
    };
  };

  # Add a user for concate
  users.users.concate = {
    isNormalUser = true;
    home = "${config.etu.dataPrefix}/home/concate";
    uid = 1001;
    openssh.authorizedKeys.keys = myData.pubkeys.concate;
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # Hardware settings
  hardware.enableRedistributableFirmware = true;

  # Enable apcupsd.
  services.apcupsd.enable = true;

  # Enable kvm
  virtualisation.libvirtd.enable = true;
  security.polkit.enable = true;

  # Set up Letsencrypt
  security.acme.defaults.email = config.etu.user.email;
  security.acme.acceptTerms = true;

  users.users.downloads = { group = "downloads"; uid = 947; isSystemUser = true; };
  users.groups.downloads.gid = 947;

  # Install some additonal tools on this system.
  environment.systemPackages = [
    pkgs.mkvtoolnix-cli  # mkvinfo and mkvmerge
    pkgs.ffmpeg          # ffmpeg
  ];
}
