{ config, lib, pkgs, ... }:

{
  options.etu.games.mumble.enable = lib.mkEnableOption "Enable games mumble settings";

  config = lib.mkIf config.etu.games.mumble.enable {
    # Install mumble for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [ pkgs.mumble ];
    };

    # Enable persistence for mumble files.
    etu.base.zfs.user.files = [
      ".config/Mumble/Mumble.conf"
      ".local/share/Mumble/Mumble/mumble.sqlite"
    ];
  };
}
