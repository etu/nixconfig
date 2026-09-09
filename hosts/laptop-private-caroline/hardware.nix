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
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t14s-amd-gen4

    # Scanned modules
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Configure boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [
    "nvme"
    "ehci_pci"
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

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Install firmware for hardware.
  hardware.enableRedistributableFirmware = true;
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # Include udev rules to give permissions to the video group to change
  # backlight using acpilight.
  hardware.acpilight.enable = true;

  # Set video driver.
  services.xserver.videoDrivers = [ "modesetting" ];

  # Enable fwupd for firmware updates etc.
  services.fwupd.enable = true;

  # TLP disabled in favor of power-profiles-daemon (enabled in dms-shell module)
  # services.tlp.enable = true;
  # services.tlp.settings.START_CHARGE_THRESH_BAT0 = 40;
  # services.tlp.settings.STOP_CHARGE_THRESH_BAT0 = 70;

  # Manually set battery charge thresholds (previously handled by TLP)
  systemd.services.battery-charge-threshold = {
    description = "Set battery charge thresholds";
    wantedBy = [ "multi-user.target" ];
    after = [ "multi-user.target" ];
    startLimitBurst = 0;
    script = ''
      echo 40 > /sys/class/power_supply/BAT0/charge_control_start_threshold
      echo 70 > /sys/class/power_supply/BAT0/charge_control_end_threshold
    '';
    serviceConfig = {
      Type = "oneshot";
    };
  };

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
