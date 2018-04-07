# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # Set up a working bootloader for Rasberry Pi 3
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # And kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Needed for the virtual console to work on the RPi 3, as the default of 16M doesn't seem to be enough.
  boot.kernelParams = [ "cma=32M" ];

  networking.hostName = "rpi-ha";

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Enable wifi.
  hardware.enableRedistributableFirmware = true;
  hardware.firmware = [
    (pkgs.stdenv.mkDerivation {
     name = "broadcom-rpi3-extra";
     src = pkgs.fetchurl {
       url = "https://raw.githubusercontent.com/RPi-Distro/firmware-nonfree/54bab3d/brcm80211/brcm/brcmfmac43430-sdio.txt";
       sha256 = "19bmdd7w0xzybfassn7x4rb30l70vynnw3c80nlapna2k57xwbw7";
     };
     phases = [ "installPhase" ];
     installPhase = ''
       mkdir -p $out/lib/firmware/brcm
       cp $src $out/lib/firmware/brcm/brcmfmac43430-sdio.txt
     '';
     })
  ];
  networking.wireless.enable = true;

  # Enable firewall.
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    git
    htop
    tmux
  ];

  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPsVycIM8SAlMRIoytIyCqdHJ+ORAiPRAMR/lo5USVeg etu@prosser-2017-07-09"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
  ];

  # Use local nixpkgs checkout
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nixpkgs"
    "nixos-config=/etc/nixos/configuration.nix"
  ];
}
