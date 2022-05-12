# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  lib,
  pkgs,
  myData,
  modulesPath,
  ...
}: {
  imports = [
    # Import bootloader and related settings for aarch64
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")

    # Include the mmdvmhost service.
    ./mmdvmhost/service.nix
  ];

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # My module settings
  etu = {
    stateVersion = "22.11";
    base.emacs.enable = lib.mkForce false;
  };

  # Cross compiling settings.
  nixpkgs.system = "aarch64-linux";

  # Add kernel params for the serial console
  boot.kernelParams = ["console=ttyS1,115200n8"];

  # Extra packages.
  environment.systemPackages = with pkgs; [
    file
    htop
  ];

  # Set hostname for system.
  networking.hostName = "nix-star";

  # Wifi.
  hardware.enableRedistributableFirmware = true;

  # Passwordless sudo for wheel.
  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  # Enable wireless networking.
  networking.wireless.enable = true;
  networking.wireless.interfaces = ["wlan0"];
  networking.wireless.networks."SSID".psk = "PASSWORD"; # Secrets

  # Create users.
  users.mutableUsers = false;
  users.users.nix-star = {
    uid = 1000;
    group = "nix-star";
    isNormalUser = true;
    initialPassword = "hunter2";
    extraGroups = ["wheel"];
    openssh.authorizedKeys.keys = myData.pubkeys.etu.computers;
  };
  users.users.root.openssh.authorizedKeys.keys = myData.pubkeys.etu.computers;

  # Enable the mmdvmhost service.
  services.mmdvmhost.enable = true;
  services.mmdvmhost.settings = let
    DMRId = "2400302";
    HSDMRId = "${DMRId}02";
  in {
    General = {
      Callsign = "SA0BSE";
      Id = HSDMRId;
      Timeout = 240;
      Duplex = 0;
      RFModeHang = 300;
      NetModeHang = 300;
      Display = "OLED";
      Daemon = 1;
    };
    Info = {
      RXFrequency = 433012500;
      TXFrequency = 433012500;
      Latitude = 59.355755; # Secrets
      Longitude = 17.883849; # Secrets
      Location = "Stockholm, JO89wi"; # Secrets
      Description = "MMDVMHost on NixOS";
      URL = "https://sa.0b.se/";
    };
    Modem = {
      UARTPort = "/dev/ttyS1";
      UARTSpeed = 115200;
      RXOffset = 7;
      TXOffset = 7;
      Debug = 1;
    };
    DMR = {
      Enable = 1;
      Id = HSDMRId;
      Debug = 1;
    };
    "DMR Network" = {
      Enable = 1;
      Type = "Direct";
      RemoteAddress = "83.233.234.102";
      Password = "BrandmeisterPassword"; # Secrets
      Slot1 = 0;
      Debug = 1;
    };
    "TFT Serial".Port = "/dev/ttyS1";
    Nextion.ScreenLayout = 0;
    Nextion.Port = "/dev/ttyS1";
    OLED = {
      Scroll = 0;
      LogoScreensaver = 0;
    };
  };
}
