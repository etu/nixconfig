{ config, lib, pkgs, ... }:
let
  cfg = config.etu.graphical.sway;

in
{
  config = lib.mkIf cfg.enable {
    # Home manager settings for sway and programs related to sway
    home-manager.users.${config.etu.user.username} = lib.mkIf config.my.home-manager.enable {
      # Configure alacritty
      programs.alacritty = {
        enable = cfg.enable;
        settings = {
          env.TERM = "xterm-256color";
          font.size = config.etu.graphical.theme.fonts.size;
          font.normal.family = config.etu.graphical.theme.fonts.monospace;
          bell = {
            duration = 250;
            color = "#441111";
            animation = "EaseOut";
          };
          colors = {
            primary = { background = "#000000"; foreground = "#dddddd"; };
            normal = {
              black = "#000000";
              red = "#cc0403";
              green = "#19cb00";
              yellow = "#cecb00";
              blue = "#0d73cc";
              magenta = "#cb1ed1";
              cyan = "#0dcdcd";
              white = "#dddddd";
            };
            bright = {
              black = "#767676";
              red = "#f2201f";
              green = "#23fd00";
              yellow = "#fffd00";
              blue = "#1a8fff";
              magenta = "#fd28ff";
              cyan = "#14ffff";
              white = "#ffffff";
            };
          };
        };
      }; # END alacritty

      programs.browserpass.enable = cfg.enable;

      xdg.mimeApps = {
        enable = cfg.enable;
        defaultApplications = {
          "text/html" = [ "firefox.desktop" ];
          "x-scheme-handler/http" = [ "firefox.desktop" ];
          "x-scheme-handler/https" = [ "firefox.desktop" ];
          "x-scheme-handler/about" = [ "firefox.desktop" ];
          "x-scheme-handler/unknown" = [ "firefox.desktop" ];
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
