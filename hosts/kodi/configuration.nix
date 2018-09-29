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
  system.stateVersion = "18.09";

  networking.hostName = "kodi";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.kernelPatches = [
    { name = "amdgpu-config";
      patch = null;
      extraConfig = ''
        DRM_AMD_DC_DCN1_0 y
      '';
    }
  ];

  # Auto upgrade system
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-18.09";

  # Auto garbage collect
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";

  # Enable some firmwares.
  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;

  # OpenGL stuff
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    lm_sensors
    nvme-cli
  ];

  # List services that you want to enable:

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Set display resolution
  services.xserver.extraDisplaySettings = ''
      Depth        24
      Modes        "1920x1080"
  '';

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

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Open port to remote control Kodi
  networking.firewall.allowedTCPPorts = [ 8080 ];

  # SSH Keys for remote logins
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
  ];
}
