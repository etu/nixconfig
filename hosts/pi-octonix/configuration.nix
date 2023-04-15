# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  myData,
  modulesPath,
  ...
}: {
  imports = [
    # Import bootloader and related settings for aarch64
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
  ];

  # Don't compress the resulting image.
  sdImage.compressImage = false;

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Set hostname for system.
  networking.hostName = "octonix";

  # My module settings
  etu = {
    stateVersion = "22.11";

    # Set data prefix so agenix can find the host keys.
    dataPrefix = "/";

    # Disable ZFS helpers to avoid persistence weirdness.
    base.zfs.enable = lib.mkForce false;

    # Disable Emacs to save some space, won't be used anyways.
    base.emacs.enable = lib.mkForce false;

    # Don't set a password for root depending on agenix.
    user.allowEmptyRootPassword = true;
  };

  # Allow root to log in without password.
  users.users.root.initialHashedPassword = "";

  # Automatically log in at the virtual consoles.
  services.getty.autologinUser = "root";

  # Wifi.
  hardware.enableRedistributableFirmware = true;

  # Enable wireless networking.
  networking.wireless.enable = true;
  networking.wireless.interfaces = ["wlan0"];
  networking.wireless.networks."SSID".psk = "PASSWORD"; # Secrets

  # Make sure octoprint is in the video group.
  users.users.octoprint.extraGroups = ["video"];

  # Enable octoprint service.
  services.octoprint.enable = true;
  services.octoprint.plugins = plugins:
    with plugins; [
      bedlevelvisualizer # bed level visualizer
      ender3v2tempfix # should contain fixes for temperature reporting from Creality printers
      themeify # theme plugin
    ];

  # Enable mjpg streamer.
  services.mjpg-streamer = {
    enable = true;
    # Lowest resulotion for better framerate.
    inputPlugin = "input_uvc.so -d /dev/video0 -r 640x480";
  };

  # Set up a proxy in front of octoprint.
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    virtualHosts."octonix.lan" = {
      default = true;
      locations."/".proxyPass = "http://127.0.0.1:5000";
      locations."/".proxyWebsockets = true;
    };
  };

  # Open port for nginx.
  networking.firewall.allowedTCPPorts = [80 5050];
}
