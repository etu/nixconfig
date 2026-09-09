{
  config,
  lib,
  modulesPath,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    # Hardware settings
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t14s-amd-gen4

    # Scanned modules
    (modulesPath + "/installer/scan/not-detected.nix")

    # Filesystem layout
    ./disko.nix
  ];

  # Configure boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "thunderbolt"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];

  boot.kernelModules = [ "kvm-amd" ];

  # Install thinkpad modules for TLP.
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

  # Enable a nice boot splash screen.
  boot.initrd.systemd.enable = true; # needed for ZFS password prompt with plymouth.
  boot.plymouth.enable = true;

  # Enable ZFS.
  boot.supportedFilesystems = [ "zfs" ];

  # Add hack to make wifi work at the office.
  boot.extraModprobeConfig = "options iwlwifi disable_11ax=Y";

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Install firmware for hardware.
  hardware.enableRedistributableFirmware = true;

  # Include udev rules to give permissions to the video group to change
  # backlight using acpilight.
  hardware.acpilight.enable = true;

  # Set video driver
  services.xserver.videoDrivers = [ "modesetting" ];

  # Enable fwupd for firmware updates etc.
  services.fwupd.enable = true;

  # Enable TLP.
  services.tlp.enable = true;
  services.tlp.settings.START_CHARGE_THRESH_BAT0 = 40;
  services.tlp.settings.STOP_CHARGE_THRESH_BAT0 = 70;

  # Mark filesystems as needed for boot
  fileSystems.${config.etu.dataPrefix}.neededForBoot = true;
  fileSystems."${config.etu.dataPrefix}/home".neededForBoot = true;
  fileSystems.${config.etu.localPrefix}.neededForBoot = true;
  fileSystems."/nix".neededForBoot = true;

  # Additional work directories
  etu.base.zfs.user.directories = [
    "tvnu"
  ];

  # Swap devices.
  swapDevices = [ ];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 8;

  # Use the host platform for building by default to avoid cross compiling.
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # Set CPU Frequency Governor.
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # High-DPI console.
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
}
