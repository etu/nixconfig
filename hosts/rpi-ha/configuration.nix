# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../profiles/common-server.nix
  ];

  # Set up a working bootloader for Rasberry Pi 3
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # And kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Needed for the virtual console to work on the RPi 3, as the default of 16M doesn't seem to be enough.
  boot.kernelParams = [ "cma=32M" ];

  networking.hostName = "rpi-ha";

  # Enable Home Assistant, open port and add the hass user to the dialout group
  services.home-assistant.enable = true;
  services.home-assistant.package = pkgs.home-assistant.override {
    extraPackages = ps: with ps; [
      # Zwave
      python_openzwave pydispatcher

      # Requirements of default modules
      netdisco distro xmltodict

      # Hue
      aiohue voluptuous-serialize
    ];
  };

  networking.firewall.allowedTCPPorts = [ 8123 ];
  users.extraUsers.hass.extraGroups = [ "dialout" ];
}
