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

    # Import the home-manager module
    <home-manager/nixos>
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.03";

  # Use local nixpkgs checkout
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nixpkgs"
    "home-manager=/nix/var/nix/profiles/per-user/root/channels/home-manager/"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  networking.hostName = "ford-x250";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.extraModulePackages = [
    pkgs.linuxPackages_latest.acpi_call
  ];

  # Fix touchpad scrolling after suspend.
  boot.kernelParams = [ "psmouse.synaptics_intertouch=0" ];

  boot.cleanTmpDir = true;

  # Hardware settings
  services.xserver.videoDrivers = ["intel" "modesetting"];
  hardware.trackpoint.enable = true;
  hardware.cpu.intel.updateMicrocode = true;

  # Enable TLP
  services.tlp.enable = true;

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

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Enable gpg related stuff
  my.gpg-utils.enable = true;

  # Enable common graphical stuff
  my.common-graphical.enable = true;

  # Enable emacs deamon stuff
  my.emacs.enable = true;

  # Enable my exwm desktop settings
  my.exwm.enable = true;

  # Enable dmrconfig to configure my hamradio.
  programs.dmrconfig.enable = true;

  # Define a user account.
  my.user.enable = true;
  my.user.extraGroups = [
    "scanner"
    "docker"
    "libvirtd"
  ];

  users.users.root.initialHashedPassword = "$6$f0a4BXeQkQ719H$5zOS.B3/gDqDN9/1Zs20JUCCPWpzkYmOx6XjPqyCe5kZD5z744iU8cwxRyNZjPRa63S2oTml7QizxfS4jjMkE1";
  users.users.etu.initialHashedPassword = "$6$f0a4BXeQkQ719H$5zOS.B3/gDqDN9/1Zs20JUCCPWpzkYmOx6XjPqyCe5kZD5z744iU8cwxRyNZjPRa63S2oTml7QizxfS4jjMkE1";

  # Home-manager as nix module
  home-manager.users.etu = import ../../home-etu-nixpkgs/home.nix;

  # Enable kvm
  virtualisation.libvirtd.enable = true;

  # Enable docker
  virtualisation.docker.enable = true;
}
