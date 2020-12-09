# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
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
  system.stateVersion = "18.09";

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "vps04";

  # Set NIX_PATH for nixos config and nixpkgs
  nix.nixPath = [ "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos" "nixos-config=/etc/nixos/hosts/vps04/configuration.nix" ];

  # Auto upgrade system
  my.auto-upgrade.enable = true;
  my.auto-upgrade.user = "etu";
  system.autoUpgrade.dates = "Mon *-*-* 04:40:00";

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

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

  # Set up users accounts:

  users.mutableUsers = false;

  users.users = {
    root.initialHashedPassword = secrets.hashedRootPassword;

    etu = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      shell = pkgs.fish;
      home = "/home/etu";
      uid = 1000;
      openssh.authorizedKeys.keys = with keys.etu; weechat ++ fenchurch ++ agrajag ++ work;
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
      isNormalUser = true;
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
