{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.common-graphical;

in {
  options.my.common-graphical.enable = mkEnableOption "Enables my common graphical thingys";

  config = mkIf cfg.enable {
    # Sane font defaults.
    fonts.enableFontDir = true;
    fonts.enableGhostscriptFonts = true;
    fonts.fontconfig.cache32Bit = true;

    # Install some extra fonts.
    fonts.fonts = with pkgs; [
      dejavu_fonts
      emacs-all-the-icons-fonts
      inconsolata
      liberation_ttf
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
      powerline-fonts
      source-code-pro
      symbola

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

    # Used for firefox-bin because of mozillas branding and pre-compiled bins
    nixpkgs.config.allowUnfree = true;

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      # Add spelling dictionaries
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      aspellDicts.sv

      dino
      feh
      firefox-bin
      mpv
      stupidterm
      kitty
      tdesktop
    ];

    # Enable pulseaudio.
    hardware.pulseaudio = {
      enable = true;
      extraModules = [ pkgs.pulseaudio-modules-bt ];

      daemon.config = {
        flat-volumes = "no";
        default-sample-format = "s24le";
        default-sample-rate = "44100";
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
    services.xserver.layout = "se";
    services.xserver.xkbOptions = "eurosign:e,ctrl:nocaps,numpad:mac,kpdl:dot";
    services.xserver.xkbVariant = "dvorak";

    # Enable U2F key support
    hardware.u2f.enable = true;

    # Enable networkmanager.
    networking.networkmanager.enable = true;

    # 1714-1764 is KDE Connect.
    networking.firewall.allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
    networking.firewall.allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];

    # 8000 is for random web sharing things.
    networking.firewall.allowedTCPPorts = [ 8000 ];

    # Define extra groups for user.
    my.user.extraGroups = [ "networkmanager" "dialout" ];
  };
}
