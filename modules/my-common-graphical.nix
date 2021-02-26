{ config, lib, pkgs, ... }:
let
  cfg = config.my.common-graphical;

in
{
  options.my.common-graphical.enable = lib.mkEnableOption "Enables my common graphical thingys";

  config = lib.mkIf cfg.enable {
    # Set up default fonts
    fonts.enableDefaultFonts = true;
    fonts.enableGhostscriptFonts = true;

    # Install some extra fonts.
    fonts.fonts = with pkgs; [
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

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      dino
      feh

      chromium
      firefox

      mpv
      stupidterm
      kitty
      tdesktop

      # Add a command to run the compose xmodmap again
      (writeScriptBin "fixcompose" ''
        #!${stdenv.shell}

        ${xorg.xmodmap}/bin/xmodmap -e 'keycode 78 = Multi_key' -e 'keycode 94 = Multi_key'
      ''
      )
    ];

    # Enable pulseaudio.
    hardware.pulseaudio = {
      enable = true;
      extraModules = [ pkgs.pulseaudio-modules-bt ];

      daemon.config = {
        flat-volumes = "no";
        # default-sample-format = "s24le";
        # default-sample-rate = "44100";
        resample-method = "speex-float-10";
        avoid-resampling = "true";
      };

      package = pkgs.pulseaudioFull;
    };

    # Enable the X11 windowing system.
    services.xserver.enable = true;

    # Don't have xterm as a session manager.
    services.xserver.desktopManager.xterm.enable = false;

    # Keyboard layout.
    services.xserver.layout = "us";
    services.xserver.xkbOptions = "eurosign:e,ctrl:nocaps,numpad:mac,kpdl:dot";
    services.xserver.xkbVariant = "dvorak";

    # Enable networkmanager.
    networking.networkmanager.enable = true;
    networking.networkmanager.wifi.backend = "iwd";

    # 8000 is for random web sharing things.
    networking.firewall.allowedTCPPorts = [ 8000 ];

    # Define extra groups for user.
    my.user.extraGroups = [ "networkmanager" "dialout" ];
  };
}
