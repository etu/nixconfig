# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    # Import local modules & overlays
    ../../overlays/local/default.nix
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

  networking.hostName = "rpi-ha";

  # Set up a working bootloader for Rasberry Pi 3
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # And kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Needed for the virtual console to work on the RPi 3, as the default of 16M doesn't seem to be enough.
  boot.kernelParams = [ "cma=32M" ];

  # Enable Home Assistant, open port and add the hass user to the dialout group
  services.home-assistant.enable = true;
  services.home-assistant.openFirewall = true;
  services.home-assistant.autoExtraComponents = false;
  services.home-assistant.package = pkgs.home-assistant.override {
    extraComponents = [
      "discovery"
      "hue"
      "media_player.cast"
      "sensor.yr"
      "updater"
      "zwave"
    ];
  };

  users.extraUsers.hass.extraGroups = [ "dialout" ];

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # SSH Keys for remote logins
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
  ];
}
