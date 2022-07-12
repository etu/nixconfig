{ config, lib, pkgs, ... }:

{
  options.etu.graphical.spotify.enable = lib.mkEnableOption "Enable graphical spotify settings";

  config = lib.mkIf config.etu.graphical.spotify.enable {
    # Allow to install spotify.
    etu.base.nix.allowUnfree = [
      "spotify"
      "spotify-unwrapped"
    ];

    # Install mumble for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [ pkgs.spotify ];
    };

    # Enable persistence for spotify files.
    environment.persistence.${config.etu.dataPrefix} = {
      users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
        files = [
          ".config/spotify/prefs"
        ];
        directories = [
          ".config/spotify/Users"
        ];
      };
    };
  };
}
