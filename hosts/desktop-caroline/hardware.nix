{
  config,
  lib,
  pkgs,
  modulesPath,
  inputs,
  ...
}:
{
  imports = [
    # Hardware settings
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-gpu-amd

    # Scanned modules
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Configure boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "thunderbolt"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];

  boot.kernelModules = [ "kvm-amd" ];

  # Use a newer kernel.
  boot.kernelPackages = pkgs.linuxPackages_6_18;

  # Enable a nice boot splash screen.
  boot.initrd.systemd.enable = true; # needed for ZFS password prompt with plymouth.
  boot.plymouth.enable = true;

  # Enable ZFS.
  boot.supportedFilesystems = [ "zfs" ];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Install firmware for hardware.
  hardware.enableRedistributableFirmware = true;

  # Test to use OpenRGB to see if it works with my RGB
  services.hardware.openrgb.enable = true;
  services.hardware.openrgb.motherboard = "amd";

  # Enable openrazer to control razer devices.
  hardware.openrazer.enable = true;
  hardware.openrazer.users = [ config.etu.user.username ];
  etu.user.extraUserPackages = [ pkgs.polychromatic ];

  # Enable fwupd for firmware updates etc.
  services.fwupd.enable = true;

  # Disko config
  disko.devices = import ./disko.nix { };

  fileSystems."/".neededForBoot = true;
  fileSystems."/home".neededForBoot = true;
  fileSystems."/nix".neededForBoot = true;
  fileSystems.${config.etu.dataPrefix}.neededForBoot = true;
  fileSystems.${config.etu.localPrefix}.neededForBoot = true;

  # Swap devices.
  swapDevices = [ ];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 8;

  # Use the host platform for building by default to avoid cross compiling.
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # Set CPU Frequency Governor.
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
