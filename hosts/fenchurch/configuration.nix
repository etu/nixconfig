# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../profiles/common.nix
    ../../profiles/xserver.nix
    ../../profiles/vbox.nix
    ../../profiles/steam.nix
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

  # Enable nvidia xserver driver
  services.xserver.videoDrivers = [ "nvidia" ];

  # Disable CUPS to print documents.
  services.printing.enable = false;

  # Enable firewall.
  networking.firewall.enable = true;
}
