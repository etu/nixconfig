# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  # Load secrets
  secrets = (import ../../data.nix).secrets;

  # Import my ssh public keys
  keys = (import ../../data.nix).pubkeys;

in {
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "21.03";

  # Set hostname
  networking.hostName = "agrajag";

  # Settings needed for ZFS
  networking.hostId = "27416952";

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;
  my.backup.enableSyncoid = true;

  # Enable snapshotting for some filesystems
  services.sanoid.datasets."zroot/home".use_template = [ "home" ];
  services.sanoid.datasets."zroot/persistent".use_template = [ "persistent" ];

  services.syncoid.commands = {
    "zroot/home".target = "root@home.elis.nu:zroot/backups/agrajag/zroot/home";
    "zroot/persistent".target = "root@home.elis.nu:zroot/backups/agrajag/zroot/persistent";
  };

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nix/nixos-unstable"
    "nixos-config=/etc/nixos/hosts/agrajag/configuration.nix"
  ];

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

  # Set up docker
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [ virt-manager ];
  virtualisation.spiceUSBRedirection.enable = true;

  # Override identity paths for agenix since the openssh default paths
  # relies on a symlink being created in /etc/ssh to point at the
  # right path to make it to work as it would be in the right place.
  age.identityPaths = [
    "/persistent/etc/ssh/ssh_host_ed25519_key"
    "/persistent/etc/ssh/ssh_host_rsa_key"
  ];

  # Include agenix encripted secret for secret password file
  age.secrets.nixos-data-secrets = {
    file = ../../secrets/workstations/nixos-data-secrets.nix.age;
    path = "/persistent/etc/nixos/.data-secrets.nix";
    owner = "etu";
  };

  # Add community server to known hosts
  programs.ssh.knownHosts."aarch64.nixos.community".publicKey = keys.systems."aarch64.nixos.community";

  age.secrets."etu@aarch64.nixos.community.age" = {
    file = ../../secrets/agrajag/etu_at_aarch64.nixos.community.age;
    path = "/persistent/home/etu/.ssh/etu@aarch64.nixos.community";
    owner = "etu";
    mode = "400";
  };

  age.secrets."etu@aarch64.nixos.community.pub.age" = {
    file = ../../secrets/agrajag/etu_at_aarch64.nixos.community.pub.age;
    path = "/persistent/home/etu/.ssh/etu@aarch64.nixos.community.pub";
    owner = "etu";
    mode = "400";
  };

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
      sshKey = config.age.secrets."etu@aarch64.nixos.community.age".path;
      sshUser = "etu";
      system = "aarch64-linux";
      supportedFeatures = [ "big-parallel" ];
    }
  ];

  # Extra binary caches
  nix.settings.substituters = [ "https://fossar.cachix.org" "https://nix-community.cachix.org" ];
  nix.settings.trusted-public-keys = [
    "fossar.cachix.org-1:Zv6FuqIboeHPWQS7ysLCJ7UT7xExb4OE8c4LyGb5AsE="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];
}
