# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:
let
  # Load secrets
  secrets = import ../../data/load-secrets.nix;

  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

in
{
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./persistence.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "20.09";

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "vps04";

  # Set up ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "8425e390";
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    htop
    irssi
    screen
    weechat
  ];

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Install tmux
  programs.tmux.enable = true;
  programs.tmux.clock24 = true;

  # Install mosh
  programs.mosh.enable = true;

  # Install fish
  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable my common cli utils
  my.common-cli.enable = true;

  # Allow password login for a user
  services.openssh.extraConfig = ''
    Match User ozeloten
      PasswordAuthentication yes
  '';
  security.pam.services.sshd.unixAuth = lib.mkForce true;
  services.openssh.challengeResponseAuthentication = false;

  # Enable my user and home-manager for my user
  my.home-manager.enable = true;
  my.user = {
    enable = true;
    extraAuthorizedKeys = keys.etu.weechat;
    persistent.extraFiles = [
      ".config/fish/fish_variables"
    ];
    persistent.extraDirectories = [
      ".dotfiles"
      ".ssh"
      ".weechat"
    ];
  };

  # Set up users accounts:

  users.mutableUsers = false;

  users.users = {
    etu.initialHashedPassword = secrets.hashedEtuPassword;

    root = {
      initialHashedPassword = secrets.hashedRootPassword;
      openssh.authorizedKeys.keys = keys.etu.computers;
    };

    concate = {
      isNormalUser = true;
      home = "/home/concate";
      uid = 1001;
      openssh.authorizedKeys.keys = keys.concate;
    };

    talyz = {
      isNormalUser = true;
      shell = pkgs.fish;
      home = "/home/talyz";
      uid = 1002;
      openssh.authorizedKeys.keys = keys.talyz;
    };

    ozeloten = {
      isNormalUser = true;
      home = "/home/ozeloten";
      initialHashedPassword = secrets.hashedOzelotenPassword;
      uid = 1003;
    };

    bots = {
      home = "/home/bots";
      uid = 1004;
    };
  };

  # Enable the flummbot service
  services.flummbot.enable = true;
  services.flummbot.user = "bots";
  services.flummbot.group = "bots";
  services.flummbot.stateDirectory = "/home/bots";

  # A hack to `loginctl enable-linger m` (for multiplexer sessions to last),
  # until this one is resolved: https://github.com/NixOS/nixpkgs/issues/3702
  system.activationScripts.loginctl-enable-linger-m = pkgs.lib.stringAfter [ "users" ] ''
    ${pkgs.systemd}/bin/loginctl enable-linger etu
    ${pkgs.systemd}/bin/loginctl enable-linger concate
    ${pkgs.systemd}/bin/loginctl enable-linger talyz
  '';
}
