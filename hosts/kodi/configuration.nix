# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;
in
{
  imports = [
    ./hardware-configuration.nix
    ./persistence.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "20.09";

  networking.hostName = "kodi";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_5_14;

  # Settings needed for ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "10227851";
  services.zfs.autoScrub.enable = true;
  services.zfs.autoScrub.interval = "Sun, 18:00:00";

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;

  # Enable snapshotting for some filesystems
  services.sanoid.datasets."zroot/home".use_template = [ "home" ];
  services.sanoid.datasets."zroot/persistent".use_template = [ "persistent" ];

  # AMD GPU drivers
  services.xserver.videoDrivers = [ "amdgpu" ];

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

  # Enable Kodi.
  services.xserver.desktopManager.kodi.enable = true;

  # Enable Kodi plugins.
  nixpkgs.config.kodi.enableSVTPlay = true;

  # Enable lightdm autologin.
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "kodi";

  # Override display manager to start after the network is up so kodi doesn't
  # try to access my network mount point before the network is up.
  systemd.services.display-manager.after = [ "network-online.target" ];
  systemd.services.display-manager.wants = [ "network-online.target" "network-pre.target" ];

  # Define a user account.
  users.extraUsers.kodi.isNormalUser = true;
  users.extraUsers.kodi.uid = 1000;

  # Enable wake on lan
  networking.interfaces.enp8s0.wakeOnLan.enable = true;

  # Make sure to kill all users processes on logout.
  services.logind.killUserProcesses = true;

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

  # Open port to remote control Kodi (8080)
  networking.firewall.allowedTCPPorts = [ 8080 ];

  # SSH Keys for remote logins
  users.users.root.openssh.authorizedKeys.keys = keys.etu.computers ++ keys.etu.syncoid;

  # Enable a user to do deployments with
  my.deploy-user.enable = true;
}
