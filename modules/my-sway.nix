{ config, lib, pkgs, ... }:
let
  cfg = config.etu.graphical.sway;

in
{
  config = lib.mkIf cfg.enable {
    # Home manager settings for sway and programs related to sway
    home-manager.users.${config.etu.user.username} = lib.mkIf cfg.enable {
      programs.browserpass.enable = cfg.enable;

      xdg.mimeApps = {
        enable = cfg.enable;
        defaultApplications = {
          "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
        };

        associations.added = {
          "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
        };
      }; # END xdg.mimeApps

      # Enable syncthing.
      services.syncthing.enable = cfg.enable;
    }; # END home-manager
  };
}
