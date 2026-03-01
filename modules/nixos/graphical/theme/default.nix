{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.graphical.theme.enable = lib.mkEnableOption "Enable graphical theme settings";
  options.etu.graphical.theme.fonts = {
    size = lib.mkOption {
      type = lib.types.int;
      default = 10;
      description = "Default font size";
    };
    biggerSize = lib.mkOption {
      type = lib.types.int;
      default = 13;
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
    # Set up default fonts
    fonts.enableDefaultPackages = true;
    fonts.enableGhostscriptFonts = true;

    # Allow to install microsoft fonts
    etu.base.nix.allowUnfree = [
      "corefonts"
    ];

    # Configure fontconfig to actually use more of Noto Color Emoji in
    # alacritty.
    fonts.fontconfig.defaultFonts.monospace = [
      config.etu.graphical.theme.fonts.monospace
      "Noto Color Emoji"
    ];

    # Install some extra fonts.
    fonts.packages = [
      pkgs.jetbrains-mono

      # Cantarell fonts seems to be used by GTK applications (was mostly
      # noticeable in Firefox UI elements).
      pkgs.cantarell-fonts

      # Install microsoft fonts
      pkgs.corefonts

      pkgs.nur.repos.etu.font-etuvetica # My own font
      pkgs.nur.repos.etu.font-talyznewroman # Install talyz's font
    ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # GTK theme configs
      gtk = {
        enable = true;
        font.name = config.etu.graphical.theme.fonts.normal;
        font.size = config.etu.graphical.theme.fonts.size;
        gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
        gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
      }; # END gtk

      # QT theme configs
      qt = {
        enable = true;
        platformTheme.name = "gtk";
      }; # END qt
    };
  };
}
