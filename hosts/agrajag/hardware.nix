{ config, lib, pkgs, ... }:

let
  # Load inputs
  sources = import ../../nix/sources.nix;

in
{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>

    # Include hardware quirks
    "${sources.nixos-hardware}/lenovo/thinkpad/t495"
  ];

  # Configure boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "nvme" "ehci_pci" "xhci_pci" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];

  boot.kernelModules = [ "kvm-amd" ];

  # Install thinkpad modules for TLP.
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

  # Set kernel.
  boot.kernelPackages = pkgs.zfs.latestCompatibleLinuxPackages;

  # Enable ZFS.
  boot.supportedFilesystems = [ "zfs" ];

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
  services.xserver.videoDrivers = [ "modesetting" ];

  # Enable fwupd for firmware updates etc.
  services.fwupd.enable = true;

  # Enable TLP.
  services.tlp.enable = true;
  services.tlp.settings.START_CHARGE_THRESH_BAT0 = 40;
  services.tlp.settings.STOP_CHARGE_THRESH_BAT0 = 70;

  # Filesystem mounts.
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=3G" "mode=755" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/D646-84B5";
    fsType = "vfat";
    options = [ "defaults" "noexec" "noauto" "x-systemd.automount" ];
  };

  fileSystems."/nix" = {
    device = "zroot/nix";
    fsType = "zfs";
  };

  fileSystems."/persistent" = {
    device = "zroot/persistent";
    fsType = "zfs";
    neededForBoot = true;
    options = [ "defaults" "noexec" ];
  };

  fileSystems."/persistent/home" = {
    device = "zroot/home";
    fsType = "zfs";
    neededForBoot = true;
    options = [ "defaults" "noexec" ];
  };

  fileSystems."/var/log" = {
    device = "zroot/var-log";
    fsType = "zfs";
    options = [ "defaults" "noexec" ];
  };

  # Bind mount for persistent libvirt state.
  etu.base.zfs.system.directories = [
    "/var/lib/libvirt"
  ];

  # Swap devices.
  swapDevices = [ ];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 8;
}
