{
  config,
  lib,
  pkgs,
  ...
}: {
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
    fonts.enableDefaultFonts = true;
    fonts.enableGhostscriptFonts = true;

    # Configure fontconfig to actually use more of Noto Color Emoji in
    # alacritty.
    fonts.fontconfig.defaultFonts.monospace = [
      config.etu.graphical.theme.fonts.monospace
      "Noto Color Emoji"
    ];

    # Install some extra fonts.
    fonts.fonts = [
      pkgs.jetbrains-mono

      # Cantarell fonts seems to be used by GTK applications (was mostly
      # noticable in Firefox UI elements).
      pkgs.cantarell-fonts

      # My own font
      (pkgs.stdenv.mkDerivation {
        pname = "font-etuvetica";
        version = "1";

        src = pkgs.fetchurl {
          url = "https://elis.nu/etuvetica/css/fonts/etuvetica.ttf";
          sha256 = "0z1wf1q7wx8ny54w6fcz91r5xx9m2496jqfybciricmwhgdkz25j";
        };

        dontUnpack = true;

        installPhase = "install --mode=644 -D $src $out/share/fonts/truetype/etuvetica.ttf";
      })

      # Install talyz's font
      (pkgs.stdenv.mkDerivation {
        pname = "font-talyz-new-roman";
        version = "1";

        src = pkgs.fetchurl {
          url = "https://talyz.github.io/talyz-new-roman/font/TalyzNewRoman.ttf";
          sha256 = "00pi45pwmm1mialb643ifvp2qf6rhgwkmbk9malmyac815abpb0g";
        };

        dontUnpack = true;

        installPhase = "install --mode=644 -D $src $out/share/fonts/truetype/talyz-new-roman.ttf";
      })
    ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # GTK theme configs
      gtk = {
        enable = true;
        font.name = config.etu.graphical.theme.fonts.normal;
        font.size = config.etu.graphical.theme.fonts.size;
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
