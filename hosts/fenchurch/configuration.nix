# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../common.nix
    ../../services/xserver.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Used for nvidia drivers
  nixpkgs.config.allowUnfree = true;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    fish
    curl
    emacs
    htop
    git
    gnupg
    ccid
    nfs-utils
  ];

  networking.hostName = "fenchurch";

  # Enable nvidia xserver driver
  services.xserver.videoDrivers = [ "nvidia" ];

  # Disable CUPS to print documents.
  services.printing.enable = false;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.etu = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.fish;
  };

  # Root shell
  users.extraUsers.root.shell = pkgs.fish;
}
