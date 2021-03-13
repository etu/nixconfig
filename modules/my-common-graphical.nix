{ config, lib, pkgs, ... }:
let
  isX11 = config.my.i3.enable || config.my.emacs.enableExwm;
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
      "DejaVu Sans Mono"
      "Noto Color Emoji"
    ];

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

      pavucontrol

      chromium
      firefox

      mpv
      stupidterm
      tdesktop

      pulseeffects-pw
    ] ++ lib.optionals isX11 [
      # Add a command to run the compose xmodmap again
      (writeScriptBin "fixcompose" ''
        #!${stdenv.shell}
        ${xorg.xmodmap}/bin/xmodmap -e 'keycode 78 = Multi_key' -e 'keycode 94 = Multi_key'
      '')
    ];

    # Set up Pipewire for audio
    services.pipewire.enable = true;
    services.pipewire.alsa.enable = true;
    services.pipewire.pulse.enable = true;

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
