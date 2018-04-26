# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../profiles/common.nix
    ../../profiles/common-graphical.nix
    ../../profiles/desktop-gnome.nix
    ../../profiles/vbox.nix
    ../../profiles/gaming.nix
  ];

  networking.hostName = "fenchurch";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # Firmware update deamon
  services.fwupd.enable = true;

  # Enable nvidia xserver driver
  services.xserver.videoDrivers = [ "nvidia" ];

  # Disable CUPS to print documents.
  services.printing.enable = false;

  # Build nix stuff with all the power
  nix.buildCores = 9;
}
