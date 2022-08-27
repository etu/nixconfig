{ lib, config, ... }:

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

  fileSystems."/nix" = {
    device = "zroot/nix";
    fsType = "zfs";
  };

  fileSystems."/var/log" = {
    device = "zroot/var-log";
    fsType = "zfs";
  };

  fileSystems.${config.etu.dataPrefix} = {
    device = "zroot/persistent";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C5E1-A34A";
    fsType = "vfat";
  };

  etu.base.zfs.system.directories = [
    # Persistence of gitea and postgresql data files between boots
    "/var/lib/gitea"
    "/var/lib/postgresql"

    # Persistence of certificates for nginx
    "/var/lib/acme"

    # Persistence of roots dotfiles between boots
    "/root"
  ];

  # Swap devices.
  swapDevices = [ ];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 1;
}
