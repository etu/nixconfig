# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    # Import local modules & overlays
    ../../overlays/local/default.nix

    # Import the home-manager module
    <home-manager/nixos>
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.09";

  # Use local nixpkgs checkout
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nixpkgs"
    "home-manager=/nix/var/nix/profiles/per-user/root/channels/home-manager/"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  networking.hostName = "fenchurch";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_4_19;

  boot.cleanTmpDir = true;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # Needed for nvidia drivers
  nixpkgs.config.allowUnfree = true;

  # Enable nvidia xserver driver
  services.xserver.videoDrivers = [ "nvidia" ];

  # Disable nvidia compositing in webkitgtk due to this bug
  # https://github.com/NixOS/nixpkgs/issues/32580
  environment.variables.WEBKIT_DISABLE_COMPOSITING_MODE = "1";

  # Disable CUPS to print documents.
  services.printing.enable = false;

  # Build nix stuff with all the power
  nix.buildCores = 9;

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

  # Enable my exwm desktop settings
  my.exwm.enable = true;

  # Define a user account.
  my.user.enable = true;
  my.user.extraGroups = [
    "libvirtd"
  ];

  # Enable virtualbox and friends.
  my.vbox.enable = true;

  # Enable kvm
  virtualisation.libvirtd.enable = true;

  # Enable dmrconfig to configure my hamradio.
  programs.dmrconfig.enable = true;

  # Enable gaming related thingys.
  my.gaming.enable = true;

  # Home-manager as nix module
  home-manager.users.etu = import ../../home-etu-nixpkgs/home.nix;
}
