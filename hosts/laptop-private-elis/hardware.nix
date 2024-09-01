{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./disko.nix
  ];

  # Configure boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "thunderbolt" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];

  boot.kernelModules = ["kvm-amd"];

  # Install thinkpad modules for TLP.
  boot.extraModulePackages = with config.boot.kernelPackages; [acpi_call];

  # Set kernel.
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

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

  # Include udev rules to give permissions to the video group to change
  # backlight using acpilight.
  hardware.acpilight.enable = true;

  # Set video driver.
  services.xserver.videoDrivers = ["modesetting"];

  # Enable fwupd for firmware updates etc.
  services.fwupd.enable = true;

  # Enable TLP.
  services.tlp.enable = true;
  services.tlp.settings.START_CHARGE_THRESH_BAT0 = 40;
  services.tlp.settings.STOP_CHARGE_THRESH_BAT0 = 70;

  # Mark filesystems as needed for boot
  fileSystems.${config.etu.dataPrefix}.neededForBoot = true;
  fileSystems."${config.etu.dataPrefix}/home".neededForBoot = true;
  fileSystems."${config.etu.dataPrefix}/flatpak-data".neededForBoot = true;
  fileSystems."/nix".neededForBoot = true;

  # Bind mount for persistent libvirt state.
  etu.base.zfs.system.directories = [
    "/var/lib/libvirt"
  ];

  # Swap devices.
  swapDevices = [];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 8;

  # Set CPU Frequency Governor.
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
