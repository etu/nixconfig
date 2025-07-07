{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Configure boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "thunderbolt" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];

  boot.kernelModules = ["kvm-amd"];

  # Install thinkpad modules for TLP.
  boot.extraModulePackages = with config.boot.kernelPackages; [acpi_call];

  # Use a newer kernel.
  boot.kernelPackages = pkgs.linuxPackages_6_15;

  # Enable a nice boot splash screen.
  boot.initrd.systemd.enable = true; # needed for ZFS password prompt with plymouth.
  boot.plymouth.enable = true;

  # Enable ZFS.
  boot.supportedFilesystems = ["zfs"];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Install firmware for hardware.
  hardware.enableRedistributableFirmware = true;
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # Test to use OpenRGB to see if it works with my RGB
  services.hardware.openrgb.enable = true;
  services.hardware.openrgb.motherboard = "amd";

  # Set video driver.
  services.xserver.videoDrivers = ["modesetting"];

  # Enable fwupd for firmware updates etc.
  services.fwupd.enable = true;

  # Disko config
  disko.devices = import ./disko.nix {};

  fileSystems."/".neededForBoot = true;
  fileSystems."/home".neededForBoot = true;
  fileSystems."/nix".neededForBoot = true;
  fileSystems.${config.etu.dataPrefix}.neededForBoot = true;
  fileSystems.${config.etu.localPrefix}.neededForBoot = true;

  # Swap devices.
  swapDevices = [];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 8;

  # Set CPU Frequency Governor.
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
