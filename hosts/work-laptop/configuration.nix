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
    stateVersion = "22.11";

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
    user.extraGroups = [ "video" "docker" ];

    # Install extra modes for work.
    base.emacs.extraConfig = [
      ''
        ;; Install Elasticsearch mode
        (use-package es-mode :ensure t)

        ;; Install Jenkinsfile mode
        (use-package jenkinsfile-mode :ensure t)

        ;; Install VCL mode
        (use-package vcl-mode :ensure t)
      ''
    ];

    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = [ "data" ];
      "zroot/safe/home".use_template = [ "home" ];
    };
    base.syncoid.enable = true;
    # Enable syncing of some filesystems
    base.syncoid.commands = {
      "zroot/safe/data".target = "root@home.elis.nu:zroot/backups/current/work/data";
      "zroot/safe/home".target = "root@home.elis.nu:zroot/backups/current/work/home";
    };

    # Enable work modules
    work.enable = true;
  };

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nix/nixos-unstable"
    "nixos-config=/etc/nixos/hosts/work-laptop/configuration.nix"
  ];

  # Enable docker deamon
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Include agenix encripted secret for secret password file
  age.secrets = {
    inherit (ageModules) nixos-data-secret syncoid-workstations-ssh-ec;
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
    maxJobs = 8;
    sshKey = config.age.secrets.syncoid-workstations-ssh-ec.path;
    sshUser = "root";
    system = "x86_64-linux";
  }];
}
