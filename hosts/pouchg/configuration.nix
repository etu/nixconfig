# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../common.nix
    ../../services/xserver.nix
  ];

  networking.hostName = "pouchg";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # Disable CUPS to print documents.
  services.printing.enable = true;

  # Enable firewall.
  networking.firewall.enable = true;
}
