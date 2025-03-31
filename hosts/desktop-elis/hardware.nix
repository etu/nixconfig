{
  config,
  pkgs,
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

  # Set boot loader timeout to longer than 5s to give me more time to choose.
  boot.loader.timeout = 15;

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "thunderbolt" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];

  boot.kernelModules = ["kvm-amd"];

  # Wifi needs at least 6.11, system works overall well with 6.12
  # (with a separate network card) and built in ethernet works
  # starting with 6.13.
  boot.kernelPackages = pkgs.linuxPackages_6_13;

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

  # Test to use OpenRGB to see if it works with my RGB
  services.hardware.openrgb.enable = true;
  services.hardware.openrgb.motherboard = "amd";

  # Set video driver.
  services.xserver.videoDrivers = ["modesetting"];

  # Enable AMDVLK for Vulkan support.
  hardware.amdgpu.amdvlk.enable = true;
  hardware.amdgpu.amdvlk.support32Bit.enable = true;

  # Enable fwupd for firmware updates etc.
  services.fwupd.enable = true;

  # Mark filesystems as needed for boot
  fileSystems.${config.etu.dataPrefix}.neededForBoot = true;
  fileSystems."${config.etu.dataPrefix}/home".neededForBoot = true;
  fileSystems.${config.etu.localPrefix}.neededForBoot = true;
  fileSystems."/nix".neededForBoot = true;

  # Bind mount for persistent libvirt state.
  etu.base.zfs.system.directories = [
    "/var/lib/libvirt"
  ];

  # Swap devices.
  swapDevices = [];

  # Set max jobs in nix.
  #nix.settings.max-jobs = lib.mkDefault 8;

  # Set CPU Frequency Governor.
  #powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
