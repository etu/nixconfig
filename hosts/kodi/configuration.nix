# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../profiles/common-server.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "kodi";

  # Enable some firmwares.
  hardware.enableRedistributableFirmware = true;

  # Enable amdgpu driver.
  boot.kernelParams = [ "amdgpu.dc=1" ]; # Shouldn't be needed but added it anyways.
  services.xserver.videoDrivers = [ "amdgpu" ];

  # OpenGL stuff
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    lm_sensors
  ];

  # List services that you want to enable:

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable Kodi.
  services.xserver.desktopManager.kodi.enable = true;

  # Enable slim autologin.
  services.xserver.displayManager.slim.enable = true;
  services.xserver.displayManager.slim.autoLogin = true;
  services.xserver.displayManager.slim.defaultUser = "kodi";

  # Define a user account.
  users.extraUsers.kodi.isNormalUser = true;
  users.extraUsers.kodi.uid = 1000;

  # Need access to use HDMI CEC Dongle
  users.extraUsers.kodi.extraGroups = [ "dialout" ];
}
