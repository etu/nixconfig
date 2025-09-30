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

  # Roll back certain filesystems to empty state on boot
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r zroot/local/nginx-cache@empty
  '';

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Filesystem mounts.
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = ["defaults" "size=1G" "mode=755"];
  };

  fileSystems."/nix" = {
    device = "zroot/local/nix";
    fsType = "zfs";
  };

  fileSystems."/var/log" = {
    device = "zroot/local/var-log";
    fsType = "zfs";
  };

  fileSystems.${config.etu.dataPrefix} = {
    device = "zroot/safe/data";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems.${config.etu.localPrefix} = {
    device = "zroot/local/data";
    fsType = "zfs";
    neededForBoot = true;
    options = ["defaults" "noexec"];
  };

  fileSystems."/var/cache/nginx" = {
    device = "zroot/local/nginx-cache";
    fsType = "zfs";
    neededForBoot = true;
    options = ["defaults" "noexec"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/2828bb52-e581-4820-989c-3c9ca18c2925";
    fsType = "ext4";
  };

  etu.base.zfs.system.directories = [
    # Persistence of roots dotfiles between boots
    "/root"
  ];

  # Swap devices.
  swapDevices = [];

  # Use the host platform for building by default to avoid cross compiling.
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 1;
}
