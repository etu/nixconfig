# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
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
  networking.hostName = "eliaxe-A100514-NR";

  # Settings needed for ZFS
  networking.hostId = "18582528";

  # My module settings
  etu = {
    stateVersion = "21.11";
    dataPrefix = "/data";

    development.enable = true;
    graphical.enable = true;
    graphical.spotify.enable = true;
    graphical.virtualbox.enable = true;
    services.nfs.enable = true;
    services.nfs.exports = ''
      ${config.etu.dataPrefix}/home/etu/tvnu/projects 192.168.5.102(rw,no_subtree_check,all_squash,anonuid=1000,anongid=100)
    '';
    services.syncthing.enable = true;
    user.enable = true;
    user.extraGroups = [ "video" "adbusers" "docker" ];
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/home".use_template = [ "home" ];
      "zroot/safe/data".use_template = [ "persistent" ];
    };
    base.syncoid.enable = true;
    # Enable syncing of some filesystems
    base.syncoid.commands = {
      "zroot/safe/home".target = "root@home.elis.nu:zroot/backups/eliaxe-A100514-NR/zroot/home";
      "zroot/safe/data".target = "root@home.elis.nu:zroot/backups/eliaxe-A100514-NR/zroot/persistent";
    };
  };

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nix/nixos-unstable"
    "nixos-config=/etc/nixos/hosts/eliaxe-A100514-NR/configuration.nix"
  ];

  # Enable docker deamon
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Install ADB for occational android device things
  programs.adb.enable = true;

  # Install netdata for system monitoring
  services.netdata.enable = true;

  # Include agenix encripted secret for secret password file
  age.secrets = {
    inherit (ageModules) nixos-data-secret;
  };

  # Enable ClamAV.
  services.clamav.daemon.enable = true;
  services.clamav.updater.enable = true;

  # Enable blueman.
  services.blueman.enable = true;

  # Set up remote builds
  nix.distributedBuilds = true;
  nix.buildMachines = [{
    hostName = "home.elis.nu";
    system = "x86_64-linux";
    maxJobs = 5;
    sshUser = "root";
    sshKey = "${config.etu.dataPrefix}/home/syncoid/.ssh/id_ed25519";
  }];
}
