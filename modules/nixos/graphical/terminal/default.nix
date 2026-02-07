{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
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
      imports = [
        flake.homeModules.alacritty
        flake.homeModules.foot
      ];
    };
  };
}
