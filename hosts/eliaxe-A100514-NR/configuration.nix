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

    development.enable = true;
    graphical.enable = true;
    graphical.spotify.enable = true;
    graphical.virtualbox.enable = true;
    user.enable = true;
    user.extraGroups = [ "video" "adbusers" "docker" ];
  };

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;
  my.backup.enableSyncoid = true;

  # Enable snapshotting for some filesystems
  services.sanoid.datasets."zroot/home".use_template = [ "home" ];
  services.sanoid.datasets."zroot/persistent".use_template = [ "persistent" ];

  services.syncoid.commands = {
    "zroot/home".target = "root@home.elis.nu:zroot/backups/eliaxe-A100514-NR/zroot/home";
    "zroot/persistent".target = "root@home.elis.nu:zroot/backups/eliaxe-A100514-NR/zroot/persistent";
  };

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nix/nixos-unstable"
    "nixos-config=/etc/nixos/hosts/eliaxe-A100514-NR/configuration.nix"
  ];

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Enable nfsd with firewall rules.
  my.nfsd.enable = true;

  # Enable docker deamon
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Enable nfs server exports.
  services.nfs.server.exports = ''
    /persistent/home/etu/tvnu/projects 192.168.5.102(rw,no_subtree_check,all_squash,anonuid=1000,anongid=100)
  '';

  # Install ADB for occational android device things
  programs.adb.enable = true;

  # Install netdata for system monitoring
  services.netdata.enable = true;

  # Override identity paths for agenix since the openssh default paths
  # relies on a symlink being created in /etc/ssh to point at the
  # right path to make it to work as it would be in the right place.
  age.identityPaths = [
    "/persistent/etc/ssh/ssh_host_ed25519_key"
    "/persistent/etc/ssh/ssh_host_rsa_key"
  ];

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
    sshKey = "/persistent/home/syncoid/.ssh/id_ed25519";
  }];

  # Extra binary caches
  nix.settings.substituters = [ "https://fossar.cachix.org" "https://nix-community.cachix.org" ];
  nix.settings.trusted-public-keys = [
    "fossar.cachix.org-1:Zv6FuqIboeHPWQS7ysLCJ7UT7xExb4OE8c4LyGb5AsE="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];
}
