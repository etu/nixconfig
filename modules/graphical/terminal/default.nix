{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.terminal = {
    enable = lib.mkEnableOption "Enable graphical terminal settings";
    terminalPackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.alacritty;
      description = "Terminal package to use.";
    };
    terminalPath = lib.mkOption {
      type = lib.types.str;
      default = "${config.etu.graphical.terminal.terminalPackage}/bin/${config.etu.graphical.terminal.terminalName}";
      description = "Path to terminal binary.";
      readOnly = true;
    };
    terminalName = lib.mkOption {
      type = lib.types.str;
      default = config.etu.graphical.terminal.terminalPackage.meta.mainProgram;
      description = "Binary name of the terminal.";
      readOnly = true;
    };
  };

  config = lib.mkIf config.etu.graphical.terminal.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Configure alacritty
      programs.alacritty = {
        enable = true;
        settings = {
          env.TERMINAL = config.etu.graphical.terminal.terminalName;
          env.TERM = "xterm-256color";
          font.size = config.etu.graphical.theme.fonts.size;
          font.normal.family = config.etu.graphical.theme.fonts.monospace;
        };
      }; # END alacritty

      programs.foot = {
        enable = true;
        settings = {
          main.term = "xterm-256color";
          main.font = "${config.etu.graphical.theme.fonts.monospace}:size=${builtins.toString config.etu.graphical.theme.fonts.size}";
          environment.TERMINAL = "foot";
        };
      }; # END foot
    }; # END home-manager
  };
}
