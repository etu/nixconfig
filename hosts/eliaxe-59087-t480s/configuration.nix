# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  # Load secrets
  secrets = import ../../data/load-secrets.nix;

  # Load sources
  sources = import ../../nix/sources.nix;

in
{
  imports = [
    # Include hardware quirks
    "${sources.nixos-hardware}/lenovo/thinkpad/t480s"

    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./persistence.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.09";

  networking.hostName = "eliaxe-59087-t480s";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_5_10;

  # Enable ZFS support
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "18582527";

  # Snapshot and scrub automatically
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true;

  # Install thinkpad modules for TLP
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nix/nixos-unstable"
    "nixos-config=/etc/nixos/hosts/eliaxe-59087-t480s/configuration.nix"
  ];

  # Hardware settings
  services.xserver.videoDrivers = [ "intel" ];
  hardware.cpu.intel.updateMicrocode = true;

  # Enable fwupd for firmware updates etc
  services.fwupd.enable = true;

  # Enable TLP
  services.tlp.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  services.hardware.bolt.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable nfs server exports.
  services.nfs.server.exports = ''
    /home/etu/tvnu/projects 192.168.5.102(rw,no_subtree_check,all_squash,anonuid=1000,anongid=100)
  '';

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable gpg related stuff
  my.gpg-utils.enable = true;

  # Enable emacs deamon stuff
  my.emacs.enable = true;
  my.emacs.package = "wayland";

  # Enable sway
  my.sway.enable = true;

  # Define a user account.
  my.user.enable = true;
  my.user.extraGroups = [ "adbusers" "docker" ];

  # Immutable users due to tmpfs
  users.mutableUsers = false;

  # Install ADB for occational android device things
  programs.adb.enable = true;

  # Enable docker deamon
  virtualisation.docker.enable = true;

  # Enable podman
  virtualisation.podman.enable = true;
  environment.systemPackages = with pkgs; [ podman-compose ];
  # For future reference: virtualisation.podman.dockerCompat = false;

  # Install netdata for system monitoring
  services.netdata.enable = true;

  # Set passwords
  users.users.root.initialHashedPassword = secrets.hashedRootPassword;
  users.users.etu.initialHashedPassword = secrets.hashedEtuPassword;

  # Enable nfsd with firewall rules.
  my.nfsd.enable = true;

  # Enable vbox and friends.
  my.vbox.enable = true;

  # Home-manager as nix module
  my.home-manager.enable = true;
}
