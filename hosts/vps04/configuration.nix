# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  # Load secrets
  secrets = import ../../data/load-secrets.nix;

  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

in {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./persistence.nix

    # Import local modules
    ../../modules
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "vps04";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Auto upgrade system
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-19.09-small";
  system.autoUpgrade.dates = "weekly";

  # Auto garbage collect
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";

  # Auto update the config before it upgrades the system
  my.update-config.enable = true;
  my.update-config.user = "etu";

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git htop screen weechat irssi
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
      initialHashedPassword = "$6$Tp21Uo367npe/$/a6taUbyYu3QQo8RAPp6krKAq8wNs67hqSc0KPCzT32N7Aqkud110qddUAywOGKdYPJc/23BqogmUpVBQEoGF/";
      uid = 1003;
    };

    bots = {
      isNormalUser = true;
      home = "/home/bots";
      uid = 1004;
    };
  };

  # Enable the flummbot service
  programs.flummbot.enable = true;
  programs.flummbot.stateDirectory = "/home/bots";

  # Enable the bridge for IX Discord / IRC
  services.matterbridge.enable = true;
  services.matterbridge.configPath = "/home/bots/matterbridge.toml";
  services.matterbridge.user = "bots";
  services.matterbridge.group = "users";

  # A hack to `loginctl enable-linger m` (for multiplexer sessions to last),
  # until this one is resolved: https://github.com/NixOS/nixpkgs/issues/3702
  system.activationScripts.loginctl-enable-linger-m = pkgs.lib.stringAfter [ "users" ] ''
    ${pkgs.systemd}/bin/loginctl enable-linger etu
    ${pkgs.systemd}/bin/loginctl enable-linger concate
    ${pkgs.systemd}/bin/loginctl enable-linger talyz
  '';

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
