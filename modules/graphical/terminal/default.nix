{ config, lib, pkgs, ... }:

{
  options.etu.graphical.terminal = {
    enable = lib.mkEnableOption "Enable graphical terminal settings";
    terminalPackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.alacritty;
      description = "Terminal package to use.";
    };
    terminalPath = lib.mkOption {
      type = lib.types.str;
      default = "${config.etu.graphical.terminal.terminalPackage}/bin/alacritty";
      description = "Path to terminal to use.";
    };
  };

  config = lib.mkIf config.etu.graphical.terminal.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Configure alacritty
      programs.alacritty = {
        enable = true;
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
    }; # END home-manager
  };
}
