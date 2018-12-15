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

  # Use local nixpkgs checkout
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nixpkgs"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  networking.hostName = "ford";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Fix touchpad scrolling after suspend.
  boot.kernelParams = [ "psmouse.synaptics_intertouch=0" ];

  # Hardware settings
  services.xserver.videoDrivers = ["intel" "modesetting"];
  hardware.trackpoint.enable = true;
  hardware.cpu.intel.updateMicrocode = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Enable pulse with all the modules
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [
    pkgs.postscript-lexmark
  ];

  # Enable SANE to handle scanners
  hardware.sane.enable = true;

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable gpg related stuff
  my.gpg-utils.enable = true;

  # Enable common graphical stuff
  my.common-graphical.enable = true;

  # Enable emacs deamon stuff
  my.emacs.enable = true;

  # Enable my i3 desktop settings
  my.i3.enable = true;

  # Enable dmrconfig to configure my hamradio.
  programs.dmrconfig.enable = true;

  # Define a user account.
  my.user.enable = true;
  my.user.extraGroups = [
    "scanner"
    "docker"
  ];

  # Enable docker
  virtualisation.docker.enable = true;
}
