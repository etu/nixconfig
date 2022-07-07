{ config, lib, ... }:

{
  imports = [
    ./emacs
    ./fish
    ./htop
    ./nix
    ./tmux
  ];

  options.etu.stateVersion = lib.mkOption {
    example = "22.05";
    description = "The NixOS state version to use for this system";
  };

  config = {
    # Enable base services.
    etu.base = {
      emacs.enable = true;
      fish.enable = true;
      htop.enable = true;
      tmux.enable = true;
      nix.enable = true;
    };

    # Set your time zone.
    time.timeZone = "Europe/Stockholm";

    # Select internationalisation properties.
    i18n = {
      defaultLocale = "en_US.UTF-8";
      supportedLocales = [
        "all"
      ];
    };

    # Set console font and keymap.
    console.font = "Lat2-Terminus16";
    console.keyMap = "dvorak";

    # Set system state version.
    system.stateVersion = config.etu.stateVersion;

    # Set state version for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.stateVersion = config.etu.stateVersion;
    };

    # Set state version for root users home-manager.
    home-manager.users.root.home.stateVersion = config.etu.stateVersion;
  };
}
