{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  # Configure boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";

  boot.initrd.availableKernelModules = ["ahci" "xhci_pci" "virtio_pci" "sd_mod" "sr_mod"];
  boot.initrd.kernelModules = [];

  boot.kernelModules = [];
  boot.extraModulePackages = [];

  # Enable ZFS.
  boot.supportedFilesystems = ["zfs"];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Filesystem mounts.
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = ["defaults" "size=1G" "mode=755"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/d915951b-f86e-4ef7-86fe-5194f02e1d0d";
    fsType = "ext4";
  };

  fileSystems."/nix" = {
    device = "zroot/local/nix";
    fsType = "zfs";
  };

  fileSystems."${config.etu.dataPrefix}/home" = {
    device = "zroot/safe/home";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems.${config.etu.dataPrefix} = {
    device = "zroot/safe/data";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/var/log" = {
    device = "zroot/local/var-log";
    fsType = "zfs";
  };

  # Persistence of certain hosts paths and home directory paths.
  etu.base.zfs.user.directories = [
    ".weechat"
  ];

  # Persistence of all users dotfiles between boots
  etu.base.zfs.system.directories = [
    "/home/bots"
    "/home/concate"
    "/home/ozeloten"
    "/home/talyz"
    "/root"
  ];

  # Swap devices.
  swapDevices = [];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 1;
}
