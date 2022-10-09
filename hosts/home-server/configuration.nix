# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  # Import my ssh public keys
  keys = (import ../../data.nix).pubkeys;

in
{
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Some networking wait tooks for certain services
    ./network-wait-hook.nix

    # Import local services that are host specific
    ./services/guest-users.nix
    ./services/hass.nix
    ./services/home-nginx.nix
    ./services/monitoring.nix
    ./services/svtplay.nix
    ./services/usenet.nix

    # Import local modules
    ../../modules
  ];

  # Set hostname
  networking.hostName = "fenchurch";

  # Enable networking.
  networking.useDHCP = true;

  # Settings needed for ZFS
  networking.hostId = "23916528";

  # My module settings
  etu = {
    stateVersion = "19.09";

    base.emacs.package = "nox";
    development.git.enable = true;
    user.enable = true;
    user.extraGroups = [ "libvirtd" ];
    user.extraRootAuthorizedKeys = keys.etu.syncoid.workstations;
    services.freshrss.enable = true;
    services.jellyfin.enable = true;
    services.syncthing.enable = true;
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = [ "data" ];
      "zroot/safe/home".use_template = [ "home" ];

      # Enable cleanup for synced backups
      "zroot/backups/agrajag/data" = { use_template = [ "data" ]; autosnap = false; };
      "zroot/backups/agrajag/home" = { use_template = [ "home" ]; autosnap = false; };
      "zroot/backups/work/data" = { use_template = [ "data" ]; autosnap = false; };
      "zroot/backups/work/home" = { use_template = [ "home" ]; autosnap = false; };
      "zroot/backups/vps04/data" = { use_template = [ "data" ]; autosnap = false; };
      "zroot/backups/vps04/home" = { use_template = [ "home" ]; autosnap = false; };
      "zroot/backups/vps05/data" = { use_template = [ "data" ]; autosnap = false; };

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
      "root@vps04.elis.nu:zroot/safe/data".target = "zroot/backups/vps04/data";
      "root@vps04.elis.nu:zroot/safe/home".target = "zroot/backups/vps04/home";
      "root@vps05.elis.nu:zroot/safe/data".target = "zroot/backups/vps05/data";
    };
  };

  # Add a user for concate
  users.users.concate = {
    isNormalUser = true;
    home = "${config.etu.dataPrefix}/home/concate";
    uid = 1001;
    openssh.authorizedKeys.keys = keys.concate;
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
}
