# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  # Load secrets
  secrets = import ../../data/load-secrets.nix;

  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

  # Load inputs
  sources = import ../../nix/sources.nix;
in {
  imports = [
    # Include hardware quirks
    "${sources.nixos-hardware}/lenovo/thinkpad/t495"

    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./persistence.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "21.03";

  # Set hostname
  networking.hostName = "agrajag";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_5_13;

  # Settings needed for ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "27416952";
  services.zfs.autoScrub.enable = true;

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;
  my.backup.enableSyncoid = true;

  # Enable snapshotting for some filesystems
  services.sanoid.datasets."zroot/home".useTemplate = [ "default" ];
  services.sanoid.datasets."zroot/persistent".useTemplate = [ "default" ];

  services.syncoid.commands = {
    "zroot/home".target = "root@home.elis.nu:zroot/backups/agrajag/zroot/home";
    "zroot/persistent".target = "root@home.elis.nu:zroot/backups/agrajag/zroot/persistent";
  };

  # Install thinkpad modules for TLP
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nix/nixos-unstable"
    "nixos-config=/etc/nixos/hosts/agrajag/configuration.nix"
  ];

  # Hardware settings
  hardware.enableRedistributableFirmware = true;

  # Include udev rules to give permissions to the video group to change
  # backlight using acpilight.
  hardware.acpilight.enable = true;

  # Set video driver
  services.xserver.videoDrivers = [ "modesetting" ];

  # Enable fwupd for firmware updates etc
  services.fwupd.enable = true;

  # Enable TLP
  services.tlp.enable = true;
  services.tlp.settings = {
    START_CHARGE_THRESH_BAT0 = 40;
    STOP_CHARGE_THRESH_BAT0 = 70;
  };

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Enable gpg related stuff
  my.gpg-utils.enable = true;

  # Enable emacs
  my.emacs.enable = true;
  my.emacs.package = "wayland";

  # Enable sway
  my.sway.enable = true;

  # Define a user account.
  my.user.enable = true;
  my.user.extraGroups = [ "video" "docker" "libvirtd" ];

  # Immutable users due to tmpfs
  users.mutableUsers = false;

  # Set passwords
  users.users.root.initialHashedPassword = secrets.hashedRootPassword;
  users.users.etu.initialHashedPassword = secrets.hashedEtuPassword;

  # Home-manager as nix module
  my.home-manager.enable = true;

  # Enable steam things
  my.gaming.enable = true;

  # Allow user mounts to specify allow others, this is useful for sudo
  # operations within fuse mounted directories.
  programs.fuse.userAllowOther = true;

  # Set up docker
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [ virt-manager ];
  virtualisation.spiceUSBRedirection.enable = true;

  # Add community server to known hosts
  programs.ssh.knownHosts."aarch64.nixos.community".publicKey = keys.systems."aarch64.nixos.community";

  # Set up remote builds
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "home.elis.nu";
      maxJobs = 8;
      sshKey = "/persistent/home/syncoid/.ssh/id_ed25519";
      sshUser = "root";
      system = "x86_64-linux";
    }
    {
      hostName = "aarch64.nixos.community";
      maxJobs = 64;
      sshKey = "/persistent/home/etu/.ssh/id_ed25519_aarch64_nixos_community";
      sshUser = "etu";
      system = "aarch64-linux";
      supportedFeatures = [ "big-parallel" ];
    }
  ];
  nix.extraOptions = ''
    builders-use-substitutes = true

    substituters = https://cache.nixos.org https://cache.nixos.org/ https://fossar.cachix.org https://nix-community.cachix.org
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= fossar.cachix.org-1:Zv6FuqIboeHPWQS7ysLCJ7UT7xExb4OE8c4LyGb5AsE= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
  '';
}
