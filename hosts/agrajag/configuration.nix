# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  # Load secrets
  secrets = (import ../../data.nix).secrets;

  # Import my ssh public keys
  keys = (import ../../data.nix).pubkeys;

  # Import age secrets paths and metadata.
  ageModules = (import ../../data.nix).ageModules;

in
{
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Import local modules
    ../../modules
  ];

  # Set hostname
  networking.hostName = "agrajag";

  # Settings needed for ZFS
  networking.hostId = "27416952";

  # My module settings
  etu = {
    stateVersion = "21.03";

    development.enable = true;
    games.enable = true;
    graphical.enable = true;
    graphical.signal.enable = true;
    services.syncthing.enable = true;
    user.enable = true;
    user.extraGroups = [ "video" "docker" "libvirtd" ];
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/home".use_template = [ "home" ];
      "zroot/persistent".use_template = [ "persistent" ];
    };
    # Enable syncing of some filesystems
    base.syncoid.enable = true;
    base.syncoid.commands = {
      "zroot/home".target = "root@home.elis.nu:zroot/backups/agrajag/zroot/home";
      "zroot/persistent".target = "root@home.elis.nu:zroot/backups/agrajag/zroot/persistent";
    };
  };

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nix/nixos-unstable"
    "nixos-config=/etc/nixos/hosts/agrajag/configuration.nix"
  ];

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

  age.secrets = {
    inherit (ageModules) "etu@aarch64.nixos.community" "etu@aarch64.nixos.community.pub" nixos-data-secret;
  };

  # Set up remote builds
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "home.elis.nu";
      maxJobs = 8;
      sshKey = "${config.etu.dataPrefix}/home/syncoid/.ssh/id_ed25519";
      sshUser = "root";
      system = "x86_64-linux";
    }
    {
      hostName = "aarch64.nixos.community";
      maxJobs = 64;
      sshKey = config.age.secrets."etu@aarch64.nixos.community".path;
      sshUser = "etu";
      system = "aarch64-linux";
      supportedFeatures = [ "big-parallel" ];
    }
  ];
}
