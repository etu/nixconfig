# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./persistence.nix

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

  # AMD GPU drivers
  boot.kernelPatches = [
    { name = "amdgpu-config";
      patch = null;
      extraConfig = ''
        DRM_AMD_DC_DCN1_0 y
      '';
    }
  ];

  services.xserver.videoDrivers = [ "amdgpu" ];

  # Auto upgrade system
  system.autoUpgrade.enable = false;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";

  # Auto garbage collect
  nix.gc.automatic = false;
  nix.gc.options = "--delete-older-than 30d";

  # Auto update the config before it upgrades the system
  my.update-config.enable = true;

  # Enable some firmwares.
  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;

  # OpenGL stuff
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

  # Build nix stuff with all the power
  nix.buildCores = 6;

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

  # Enable Kodi plugins.
  nixpkgs.config.kodi.enableSVTPlay = true;

  # Enable lightdm autologin.
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.user = "kodi";

  # Define a user account.
  users.extraUsers.kodi.isNormalUser = true;
  users.extraUsers.kodi.uid = 1000;

  # Need access to use HDMI CEC Dongle
  users.extraUsers.kodi.extraGroups = [ "dialout" ];

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable avahi for auto discovery of Kodi
  services.avahi.enable = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;

  # Open ports for avahi zeroconf
  networking.firewall.allowedUDPPorts = [ 5353 ];

  # Open port to remote control Kodi (8080) and Magic Mirror (9000)
  networking.firewall.allowedTCPPorts = [ 8080 9000 ];

  # Networkmanager with network online target
  # This is a hack to make my NFS not fail to mount 5 times before I get an IP
  # by DHCP on boot: https://github.com/NixOS/nixpkgs/pull/60954
  networking.networkmanager.enable = true;
  systemd.services.NetworkManager-wait-online = {
    wantedBy = [ "network-online.target" ];
  };

  # Enable Home Assistant, open port and add the hass user to the dialout group
  services.home-assistant.enable = true;
  services.home-assistant.openFirewall = true;
  services.home-assistant.autoExtraComponents = false;
  services.home-assistant.package = pkgs.home-assistant.override {
    extraComponents = [
      "cast"
      "discovery"
      "hue"
      "kodi"
      "media_player"
      "notify"
      "system_health"
      "yr"
      "zwave"
    ];
  };
  users.users.hass.extraGroups = [ "dialout" ];

  # Run docker container with the magic mirror software
  docker-containers.magic-mirror = {
    image = "bastilimbach/docker-magicmirror";
    ports = [ "9000:8080" ];
    volumes = [
      "/nix/persistent/var/lib/magic_mirror/config:/opt/magic_mirror/config"
      "/nix/persistent/var/lib/magic_mirror/modules:/opt/magic_mirror/modules"
    ];
  };

  # SSH Keys for remote logins
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIEWP6z+bCjt2XRO+mFraSRx4lrwVCVysYzruC14aQmD etu@ford-x250-2019-04-19"
  ];
}
