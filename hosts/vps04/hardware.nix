{ config, lib, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
  ];

  # Configure boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];

  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # Enable ZFS.
  boot.supportedFilesystems = [ "zfs" ];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Filesystem mounts.
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=1G" "mode=755" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/d915951b-f86e-4ef7-86fe-5194f02e1d0d";
    fsType = "ext4";
  };

  fileSystems."/nix" = {
    device = "zroot/nix";
    fsType = "zfs";
  };

  fileSystems."/persistent/home" = {
    device = "zroot/home";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/persistent" = {
    device = "zroot/persistent";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/var/log" = {
    device = "zroot/var-log";
    fsType = "zfs";
  };

  # Persistence of certain hosts paths and home directory paths.
  environment.persistence."/persistent".users.${config.etu.user.username}.directories = [
    ".dotfiles"
    ".ssh"
    ".weechat"
  ];

  # Persistence of all users dotfiles between boots
  fileSystems."/home/bots" = {
    device = "/persistent/home/bots";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  fileSystems."/home/concate" = {
    device = "/persistent/home/concate";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  fileSystems."/home/talyz" = {
    device = "/persistent/home/talyz";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  fileSystems."/home/ozeloten" = {
    device = "/persistent/home/ozeloten";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Persistence of roots dotfiles between boots
  fileSystems."/root" = {
    device = "/persistent/home/root";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Swap devices.
  swapDevices = [ ];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 1;
}
