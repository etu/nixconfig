# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

in {
  imports = [
    ./hardware-configuration.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.03";

  networking.hostName = "rpi-ha";

  # Set up a working bootloader for Rasberry Pi 3
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # And kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Needed for the virtual console to work on the RPi 3, as the default of 16M doesn't seem to be enough.
  boot.kernelParams = [ "cma=32M" ];

  boot.cleanTmpDir = true;

  # Auto upgrade system
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-19.03-small";
  system.autoUpgrade.dates = "weekly";

  # Auto garbage collect
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";

  # Auto update the config before it upgrades the system
  my.update-config.enable = true;

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "aarch64.nixos.community";
      maxJobs = 64;
      sshKey = "/root/.ssh/id_ed25519";
      sshUser = "etu";
      system = "aarch64-linux";
      supportedFeatures = [ "big-parallel" ];
    }
  ];

  # SSH Keys for remote logins
  users.extraUsers.root.openssh.authorizedKeys.keys = with keys.etu; hactar ++ agrajag ++ ford-x250;
}
