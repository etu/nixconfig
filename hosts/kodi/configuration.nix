# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

  kodipkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/44442c4ad1d1526c98f2501b2755083434a69c5b.tar.gz";
      sha256 = "0hqjii3qj7ddk7w70yxaawvdis4ag2b012djsas4zg1aab0zqvkr";
    }) { };

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
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Settings needed for ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "10227851";
  services.zfs.autoScrub.enable = true;
  services.zfs.autoScrub.interval = "Sun, 18:00:00";

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;

  # Enable snapshotting for some filesystems
  services.sanoid.datasets."zroot/home".useTemplate = [ "default" ];
  services.sanoid.datasets."zroot/persistent".useTemplate = [ "default" ];

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
  services.xserver.desktopManager.kodi.package = kodipkgs.kodi;

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
  services.wakeonlan.interfaces = [ { interface = "enp8s0"; method = "magicpacket"; } ];
  systemd.targets.multi-user.wants = [ "post-resume.service" ];

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
