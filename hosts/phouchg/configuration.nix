# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../profiles/common.nix
    ../../profiles/common-graphical.nix
    ../../profiles/desktop-plasma.nix
    ../../profiles/vbox.nix
    ../../profiles/nfsd.nix
  ];

  networking.hostName = "phouchg";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Hardware settings
  services.xserver.videoDrivers = ["intel" "modesetting"];
  hardware.cpu.intel.updateMicrocode = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Enable pulse with all the modules
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable nfs server.
  services.nfs.server.exports = ''
    /home/etu/tvnu/projects 192.168.5.102(rw,no_subtree_check,all_squash,anonuid=1000,anongid=100)
  '';
}
