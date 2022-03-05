{ config, lib, pkgs, ... }:
let
  isX11 = config.my.emacs.enableExwm;
  isWayland = config.my.sway.enable;
  isGraphical = isX11 || isWayland;

in
{
  config = lib.mkIf isGraphical {
    # Set up default fonts
    fonts.enableDefaultFonts = true;
    fonts.enableGhostscriptFonts = true;

    # Configure fontconfig to actually use more of Noto Color Emoji in
    # alacritty.
    fonts.fontconfig.defaultFonts.monospace = [
      config.my.fonts.monospace
      "Noto Color Emoji"
    ];

    # Install some extra fonts.
    fonts.fonts = with pkgs; [
      jetbrains-mono

      # Cantarell fonts seems to be used by GTK applications (was mostly
      # noticable in Firefox UI elements).
      cantarell-fonts

      # My own font
      (stdenv.mkDerivation rec {
        pname = "font-etuvetica";
        version = "1";

        src = pkgs.fetchurl {
          url = "https://elis.nu/etuvetica/css/fonts/etuvetica.ttf";
          sha256 = "0z1wf1q7wx8ny54w6fcz91r5xx9m2496jqfybciricmwhgdkz25j";
        };

        unpackPhase = ":";

        installPhase = ''
          install --mode=644 -D ${src} $out/share/fonts/truetype/etuvetica.ttf
        '';
      })

      # Install talyz's font
      (stdenv.mkDerivation rec {
        pname = "font-talyz-new-roman";
        version = "1";

        src = pkgs.fetchurl {
          url = "https://talyz.github.io/talyz-new-roman/font/TalyzNewRoman.ttf";
          sha256 = "00pi45pwmm1mialb643ifvp2qf6rhgwkmbk9malmyac815abpb0g";
        };

        unpackPhase = ":";

        installPhase = ''
          install --mode=644 -D ${src} $out/share/fonts/truetype/talyz-new-roman.ttf
        '';
      })
    ];
  };
}
