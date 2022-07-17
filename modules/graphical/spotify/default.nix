{ config, lib, pkgs, ... }:

{
  options.etu.graphical.spotify.enable = lib.mkEnableOption "Enable graphical spotify settings";

  config = lib.mkIf config.etu.graphical.spotify.enable {
    # Allow to install spotify.
    etu.base.nix.allowUnfree = [
      "spotify"
      "spotify-unwrapped"
    ];

    # Install spotify for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [ pkgs.spotify ];
    };

    # Enable persistence for spotify files.
    etu.base.zfs.user.files = [
      ".config/spotify/prefs"
    ];
    etu.base.zfs.user.directories = [
      ".config/spotify/Users"
    ];
  };
}
