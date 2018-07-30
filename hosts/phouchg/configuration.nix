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
  system.nixos.stateVersion = "18.09";

  networking.hostName = "phouchg";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelPackages = pkgs.linuxPackages_4_14;

  # Hardware settings
  services.xserver.videoDrivers = ["intel" "modesetting"];
  hardware.cpu.intel.updateMicrocode = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Enable pulse with all the modules
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable nfs server exports.
  services.nfs.server.exports = ''
    /home/etu/tvnu/projects 192.168.5.102(rw,no_subtree_check,all_squash,anonuid=1000,anongid=100)
  '';

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable gpg related stuff
  my.gpg-utils.enable = true;

  # Enable common graphical stuff
  my.common-graphical.enable = true;

  # Enable my gnome desktop settings
  my.desktop-gnome.enable = true;

  # Define a user account.
  my.user.enable = true;

  # Enable nfsd with firewall rules.
  my.nfsd.enable = true;

  # Enable vbox and friends.
  my.vbox.enable = true;
}
