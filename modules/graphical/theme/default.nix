{ config, lib, ... }:

{
  options.etu.graphical.theme.enable = lib.mkEnableOption "Enable graphical theme settings";
  options.etu.graphical.theme.fonts = {
    size = lib.mkOption {
      type = lib.types.float;
      default = 10.0;
      description = "Default font size";
    };
    biggerSize = lib.mkOption {
      type = lib.types.float;
      default = 13.0;
      description = "Bigger font size";
    };
    monospace = lib.mkOption {
      type = lib.types.str;
      default = "JetBrainsMono";
      description = "Default monospace font to use";
    };
    normal = lib.mkOption {
      type = lib.types.str;
      default = "DejaVu Sans";
      description = "Default font to use";
    };
  };

  config = lib.mkIf config.etu.graphical.enable {
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # GTK theme configs
      gtk = {
        enable = true;
        font.name = config.etu.graphical.theme.fonts.normal;
        font.size = builtins.floor config.etu.graphical.theme.fonts.size;
        gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
      }; # END gtk

      # QT theme configs
      qt = {
        enable = true;
        platformTheme = "gtk";
      }; # END qt
    };
  };
}
