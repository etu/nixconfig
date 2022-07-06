# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  # Load secrets
  secrets = (import ../../data.nix).secrets;

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
    ./services/freshrss.nix
    ./services/guest-users.nix
    ./services/hass.nix
    ./services/home-nginx.nix
    ./services/jellyfin.nix
    ./services/murmur.nix
    ./services/monitoring.nix
    ./services/nfs.nix
    ./services/svtplay.nix
    ./services/syncthing.nix
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
  };

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;
  my.backup.enableSyncoid = true;

  # Enable snapshotting for some filesystems
  services.sanoid.datasets."zroot/home".use_template = [ "home" ];
  services.sanoid.datasets."zroot/persistent".use_template = [ "persistent" ];

  # Enable cleanup for synced backups
  services.sanoid.datasets."zroot/backups/agrajag/zroot/home" = { use_template = [ "home" ]; autosnap = false; };
  services.sanoid.datasets."zroot/backups/agrajag/zroot/persistent" = { use_template = [ "persistent" ]; autosnap = false; };
  services.sanoid.datasets."zroot/backups/eliaxe-A100514-NR/zroot/home" = { use_template = [ "home" ]; autosnap = false; };
  services.sanoid.datasets."zroot/backups/eliaxe-A100514-NR/zroot/persistent" = { use_template = [ "persistent" ]; autosnap = false; };
  services.sanoid.datasets."zroot/backups/vps04/zroot/home" = { use_template = [ "home" ]; autosnap = false; };
  services.sanoid.datasets."zroot/backups/vps04/zroot/persistent" = { use_template = [ "persistent" ]; autosnap = false; };
  services.sanoid.datasets."zroot/backups/vps05/zroot/home" = { use_template = [ "home" ]; autosnap = false; };
  services.sanoid.datasets."zroot/backups/vps05/zroot/persistent" = { use_template = [ "persistent" ]; autosnap = false; };

  # Enable snapshotting for bulk storage
  services.sanoid.datasets."zstorage/files".use_template = [ "storage" ];
  services.sanoid.datasets."zstorage/files/audio".use_template = [ "storage" ];
  services.sanoid.datasets."zstorage/files/ebooks".use_template = [ "storage" ];
  services.sanoid.datasets."zstorage/files/software".use_template = [ "storage" ];
  services.sanoid.datasets."zstorage/files/upload".use_template = [ "storage" ];
  services.sanoid.datasets."zstorage/files/video".use_template = [ "storage" ];

  services.syncoid.commands = {
    "root@vps04.elis.nu:zroot/home".target = "zroot/backups/vps04/zroot/home";
    "root@vps04.elis.nu:zroot/persistent".target = "zroot/backups/vps04/zroot/persistent";
    "root@vps05.elis.nu:zroot/persistent".target = "zroot/backups/vps05/zroot/persistent";
  };

  # Allow syncoid on other computers to sync here.
  users.users.root.openssh.authorizedKeys.keys = keys.etu.syncoid;

  # Override identity paths for agenix since the openssh default paths
  # relies on a symlink being created in /etc/ssh to point at the
  # right path to make it to work as it would be in the right place.
  age.identityPaths = [
    "/persistent/etc/ssh/ssh_host_ed25519_key"
    "/persistent/etc/ssh/ssh_host_rsa_key"
  ];

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

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable a user to do deployments with
  my.deploy-user.enable = true;

  # Immutable users due to tmpfs
  users.mutableUsers = false;

  # Set passwords
  users.users.root.initialHashedPassword = secrets.hashedRootPassword;
  users.users.etu.initialHashedPassword = secrets.hashedEtuPassword;

  # Home-manager as nix module
  my.home-manager.enable = true;

  # Enable kvm
  virtualisation.libvirtd.enable = true;

  # Set up Letsencrypt
  security.acme.defaults.email = config.etu.user.email;
  security.acme.acceptTerms = true;

  users.users.downloads = { group = "downloads"; uid = 947; isSystemUser = true; };
  users.groups.downloads.gid = 947;
}
