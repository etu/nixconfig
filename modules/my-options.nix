{ config, lib, pkgs, ... }:
{
  options.my = {
    backup = {
      enable = lib.mkEnableOption "Enables backup related thingys";
      enableSanoid = lib.mkEnableOption "Enables snapshot creation";
      enableSyncoid = lib.mkEnableOption "Enables snapshot syncing";
    };
    common-cli.enable = lib.mkEnableOption "Enables my common CLI thingys";
    deploy-user.enable = lib.mkEnableOption "Enables my deploy user";
    emacs = {
      enable = lib.mkEnableOption "Enables emacs with the modules I want";
      enableWork = lib.mkEnableOption "Enables install of work related modules";
      package = lib.mkOption {
        type = lib.types.str;
        default = "default";
        defaultText = "default";
        description = "Which emacs package to use.";
      };
    };
    fonts = {
      size = lib.mkOption {
        type = lib.types.float;
        default = 10.0;
        description = "Default font size";
      };
      biggerSize = lib.mkOption {
        type = lib.types.float;
        default = 13.0;
        description = "Default font size";
      };
      monospace = lib.mkOption {
        type = lib.types.str;
        default = "JetBrainsMono";
        description = "Which default font to use";
      };
      normal = lib.mkOption {
        type = lib.types.str;
        default = "DejaVu Sans";
        description = "Default non monospace font to use";
      };
    };
    gaming.enable = lib.mkEnableOption "Enables gaming related thingys";
    gpg-utils.enable = lib.mkEnableOption "Enables smartcard and gpg related utils that I use";
    home-manager.enable = lib.mkEnableOption "Enables my home-manager config";
    nfsd.enable = lib.mkEnableOption "Enables nfsd and configures ports and stuff";
    spell.enable = lib.mkEnableOption "Enable and install aspell and hunspell with swedish and english dictionary";
    sway = {
      enable = lib.mkEnableOption "Enables sway and auto login for my user";
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.sway;
        description = "Which sway package to use";
      };
      lockCommand = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.swaylock}/bin/swaylock -f -k -i ${config.my.sway.wallpaperPackage}/dark.jpg";
        description = "Lock screen command";
      };
      xkbFile = lib.mkOption {
        type = lib.types.package;
        default = pkgs.writeText "us-dvorak-compose" ''
          // This file defines my own custom keymap. More information about which
          // parts that gets included are available in the different subfolders in:
          // ${pkgs.xorg.xkeyboardconfig}/share/X11/xkb/
          xkb_keymap {
            xkb_keycodes { include "evdev+aliases(qwerty)" };
            xkb_types    { include "default" };
            xkb_compat   { include "complete" };
            xkb_symbols  {
              include "pc+us(dvorak)+inet(evdev)+ctrl(nocaps)+eurosign(e)+kpdl(dot)"

              // Less than/Greater than/Pipe key on Swedish keyboards becomes Compose
              replace key <LSGT> { [ Multi_key ] };

              // Scroll Lock becomes Compose
              replace key <SCLK> { [ Multi_key ] };
            };
            xkb_geometry { include "pc(pc105)" };
          };
        '';
      };
      wallpaperPackage = lib.mkOption {
        type = lib.types.package;
        default = pkgs.stdenv.mkDerivation {
          pname = "wallpaper";
          version = "2021-04-01";

          src = pkgs.fetchFromGitHub {
            owner = "hexive";
            repo = "sunpaper";
            rev = "8d518dfddb5e80215ef3b884ff009df1d4bb74c2";
            sha256 = "sha256-sCG7igD2ZwfHoRpR3Kw7dAded4hG2RbMLR/9nH+nZh8=";
          };

          installPhase = let
            # upper left corner of rectangle.
            base = { x = 3835; y = 35; };
            rect = {
              xy0 = "${toString base.x},${toString base.y}";
              # Add 720p to the coordinates to find the lower right corner.
              xy1 = "${toString (base.x + 1280)},${toString (base.y + 720)}";
            };
            # Add some pixels to the base coordinates to place the text nicely.
            text.xy = "${toString (base.x + 1115)},${toString (base.y + 60)}";
          in ''
            mkdir -p $out

            # Resize for normal background
            ${pkgs.graphicsmagick}/bin/gm convert -crop 7680x2160+0+375 -resize 5120x1440 images/Lakeside/5.jpg $out/default.jpg

            # Resize a darker variation for screen locker
            ${pkgs.graphicsmagick}/bin/gm convert -crop 7680x2160+0+375 -resize 5120x1440 images/Lakeside/8.jpg $out/dark.jpg

            # Draw a 720p rectangle on top
            ${pkgs.graphicsmagick}/bin/gm convert -fill '#FFFFFFBB' -draw 'rectangle ${rect.xy0} ${rect.xy1}' $out/default.jpg 720pfigure.jpg

            # Draw a text on top of this
            ${pkgs.graphicsmagick}/bin/gm convert -fill '#FFFFFF' -pointsize 72 -draw 'text ${text.xy} "720p"' 720pfigure.jpg $out/720pfigure.jpg
          '';
        };
        description = "Which wallpaper package to use";
      };
    };
    user = {
      enable = lib.mkEnableOption "Enables my user";
      uid = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = 1000;
        description = "My user id for this system.";
      };
      username = lib.mkOption {
        type = lib.types.str;
        default = "etu";
        description = "My username for this system.";
      };
      realname = lib.mkOption {
        type = lib.types.str;
        default = "Elis Hirwing";
        description = "My realname for this system.";
      };
      email = lib.mkOption {
        type = lib.types.str;
        default = "elis@hirwing.se";
        description = "My email for this system.";
      };
      workEmail = lib.mkOption {
        type = lib.types.str;
        default = "elis.hirwing@schibsted.com";
        description = "My email for this system.";
      };
      signingKey = lib.mkOption {
        type = lib.types.str;
        default = "67FE98F28C44CF221828E12FD57EFA625C9A925F";
        description = "My public signing key for this system.";
      };
      extraGroups = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
      extraAuthorizedKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Additional authorized keys.";
      };
    };
    vbox.enable = lib.mkEnableOption "Enables vbox and related things I use";
  };
}
