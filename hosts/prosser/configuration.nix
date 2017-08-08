# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../profiles/common.nix
    ../../profiles/xserver.nix
  ];

  networking.hostName = "prosser";

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;
  hardware.trackpoint.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
}
