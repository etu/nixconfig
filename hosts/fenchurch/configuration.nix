# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../services/common.nix
    ../../services/xserver.nix
    ../../services/vbox.nix
  ];

  networking.hostName = "fenchurch";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Used for nvidia drivers
  nixpkgs.config.allowUnfree = true;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # Enable 32bit libs for steam and such.
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  # Enable nvidia xserver driver
  services.xserver.videoDrivers = [ "nvidia" ];

  # Disable CUPS to print documents.
  services.printing.enable = false;

  # Enable firewall.
  networking.firewall.enable = true;
}
