# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  # Load secrets
  secrets = import ../../data/load-secrets.nix;

  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

in
{
  imports = [
    ./hardware-configuration.nix
    ./persistence.nix
    ./raid-target.nix

    # Import local services that are host specific
    ./services/freshrss.nix
    ./services/guest-users.nix
    ./services/hass.nix
    ./services/jellyfin.nix
    ./services/magic-mirror.nix
    ./services/netdata.nix
    ./services/nfs.nix
    ./services/usenet.nix
    ./services/home-nginx.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.09";

  # Set hostname
  networking.hostName = "fenchurch";

  # Boot loader settings
  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.extraInstallCommands = ''
    mkdir -p /boot/EFI/Microsoft/Boot/ /boot-fallback/EFI/Microsoft/Boot/
    cp /boot/EFI/grub/grubx64.efi /boot/EFI/Microsoft/Boot/bootmgfw.efi
    cp /boot-fallback/EFI/grub/grubx64.efi /boot-fallback/EFI/Microsoft/Boot/bootmgfw.efi
  '';

  boot.loader.grub.mirroredBoots = [
    { devices = [ "/dev/disk/by-uuid/6258-01A0" ]; path = "/boot-fallback"; }
  ];

  # Remote unlocking of encrypted ZFS
  boot.initrd = {
    kernelModules = [ "e1000e" ];
    network.enable = true;
    # Listen to ssh to let me decrypt zfs
    network.ssh = {
      enable = true;
      port = 2222;
      hostKeys = [
        /persistent/etc/initrd-ssh/ssh_host_rsa_key
        /persistent/etc/initrd-ssh/ssh_host_ed_25519_key
      ];
      authorizedKeys = config.users.users.etu.openssh.authorizedKeys.keys;
    };
    # Prompt me for password to decrypt zfs
    network.postCommands = ''
      echo "zfs load-key -a; killall zfs" >> /root/.profile
    '';
  };
  networking.useDHCP = true;

  # Roll back certain filesystems to empty state on boot
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r zroot/var-lib-nzbget-dst@empty
  '';

  # Settings needed for ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "23916528";

  # ZFS scrubbing and snapshotting
  services.zfs.autoScrub.enable = true;

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;
  my.backup.enableSyncoid = true;
  my.backup.filesystems = [
    "zroot/home"
    "zroot/persistent"
  ];

  services.syncoid.commands = {
    "root@vps04.elis.nu:zroot/home".target = "zroot/backups/vps04/zroot/home";
    "root@vps04.elis.nu:zroot/persistent".target = "zroot/backups/vps04/zroot/persistent";
    "root@vps05.elis.nu:zroot/persistent".target = "zroot/backups/vps05/zroot/persistent";
    "root@192.168.0.105:zroot/home".target = "zroot/backups/kodi/zroot/home";
    "root@192.168.0.105:zroot/persistent".target = "zroot/backups/kodi/zroot/persistent";
  };

  # Allow syncoid on other computers to sync here.
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICdtbbrBHT1i29nFdaf54zsJ4Yrt2tOLnNotaRIneazp root@agrajag-2021-07-11"
  ];

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nix/nixos-unstable"
    "nixos-config=/etc/nixos/hosts/fenchurch/configuration.nix"
  ];

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

  # Enable emacs deamon stuff
  my.emacs.enable = true;
  my.emacs.package = "nox";

  # Define a user account.
  my.user.enable = true;
  my.user.extraGroups = [ "libvirtd" ];

  # Immutable users due to tmpfs
  users.mutableUsers = false;

  # Set passwords
  users.users.root.initialHashedPassword = secrets.hashedRootPassword;
  users.users.etu.initialHashedPassword = secrets.hashedEtuPassword;

  # Add account for concate
  users.users.concate = {
    isNormalUser = true;
    home = "/home/concate";
    uid = 1001;
    openssh.authorizedKeys.keys = keys.concate;
  };

  # Home-manager as nix module
  my.home-manager.enable = true;

  # Enable kvm
  virtualisation.libvirtd.enable = true;

  # Set up Letsencrypt
  security.acme.email = "elis@hirwing.se";
  security.acme.acceptTerms = true;

  users.users.downloads = { group = "downloads"; uid = 947; isSystemUser = true; };
  users.groups.downloads.gid = 947;
}
