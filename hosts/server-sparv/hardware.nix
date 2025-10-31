{
  config,
  lib,
  modulesPath,
  inputs,
  ...
}:
{
  imports = [
    # Hardware settings
    inputs.nixos-hardware.nixosModules.common-cpu-amd

    # Scanned modules
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Configure boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";
  boot.loader.efi.canTouchEfiVariables = true;

  boot.loader.grub.mirroredBoots = [
    {
      devices = [ "/dev/disk/by-uuid/ata-Samsung_SSD_850_EVO_250GB_S21PNSAFC51888N-part1" ];
      path = "/boot-fallback";
    }
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "usb_storage"
    "usbhid"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # Enable ZFS.
  boot.supportedFilesystems = [ "zfs" ];

  # Tune some ZFS parameters to use more RAM.
  boot.kernelParams = [
    # Enable a bigger ARC max size, reserve 30GiB.
    "zfs.zfs_arc_max=${builtins.toString (30 * 1024 * 1024 * 1024)}"
    # Enable a bigger ARC target size, reserve 28GiB.
    "zfs.zfs_arc_min=${builtins.toString (28 * 1024 * 1024 * 1024)}"
  ];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  fileSystems."/" = {
    device = "zroot/safe/root";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems.${config.etu.localPrefix} = {
    device = "zroot/local/data";
    fsType = "zfs";
    neededForBoot = true;
    options = [
      "defaults"
      "noexec"
    ];
  };

  fileSystems."/nix" = {
    device = "zroot/local/nix";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "zroot/safe/home";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/var/log" = {
    device = "zroot/local/var-log";
    fsType = "zfs";
  };

  # Make sure to import ZFS pool for cache.
  #fileSystems."/media/zstorage/lancache" = {
  #  device = "zstorage/lancache";
  #  fsType = "zfs";
  #  neededForBoot = true;
  #};

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/2A9D-C192";
    fsType = "vfat";
  };

  fileSystems."/boot-fallback" = {
    device = "/dev/disk/by-uuid/2AC0-C92B";
    fsType = "vfat";
  };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp2s0.useDHCP = lib.mkDefault true;

  # Use the host platform for building by default to avoid cross compiling.
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
