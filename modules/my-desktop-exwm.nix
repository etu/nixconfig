{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.desktop-exwm;
  i3lockCommand = "${pkgs.i3lock-pixeled}/bin/i3lock-pixeled --nofork";

  myExwmInitPlain = pkgs.writeText "emacs-exwm-unsubstituted.el" (
    builtins.readFile ./emacs-files/exwm.el
  );

  myExwmInit = (pkgs.runCommand "emacs-exwm.el" (with pkgs; {
    inherit systemd kitty flameshot i3lockCommand;
    xbacklight = xorg.xbacklight;
  }) ''
    substituteAll ${myExwmInitPlain} $out
  '');

  # Wrap up i3-battery-popup script to send notifications on low battery
  i3BatteryPopup = pkgs.stdenv.mkDerivation rec {
    pname = "i3-battery-popup";
    version = "unstable-2019-06-07";

    src = pkgs.fetchFromGitHub {
      owner = "rjekker";
      repo = "i3-battery-popup";
      rev = "d894a102a1ff95019fc59d0a19c89687d502cd1a";
      sha256 = "12ym2mlv9jk1n8cigjxbqpffch4m4s5vj5rfmf9w2rl5l6g9b8vf";
    };

    nativeBuildInputs = with pkgs; [ makeWrapper ];
    buildInputs = with pkgs; [ coreutils gawk gnugrep i3 libnotify pulseaudio tk ];

    installPhase = ''
      install -D i3-battery-popup $out/bin/i3-battery-popup

      substituteInPlace $out/bin/i3-battery-popup \
        --replace /usr/share/icons/gnome ${pkgs.gnome3.defaultIconTheme}/share/icons/Adwaita \
        --replace /usr/share/icons/elementary-xfce ${pkgs.elementary-xfce-icon-theme}/share/icons/elementary-xfce

      wrapProgram $out/bin/i3-battery-popup \
        --prefix PATH ':' ${pkgs.stdenv.lib.makeBinPath buildInputs}
    '';
  };

in {
  options.my.desktop-exwm.enable = mkEnableOption "Enables exwm and auto login for my user";

  config = mkIf cfg.enable {
    # Import the emacs overlay from nix community to get the latest
    # and greatest packages.
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];

    # Libinput
    services.xserver.libinput.enable = true;

    # Loginmanager
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.autoLogin.enable = true;
    services.xserver.displayManager.lightdm.autoLogin.user = config.my.user.username;

    # Needed for autologin
    services.xserver.desktopManager.default = "none";
    services.xserver.windowManager.default = "exwm";

    # Start i3-battery-popup for my user
    systemd.user.services.i3-battery-popup = {
      description = "i3-battery-popup";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${i3BatteryPopup}/bin/i3-battery-popup -n";
        Restart = "always";
      };
    };

    # Enable i3lock on suspend
    systemd.services.i3lock = {
      description = "Lock screen before suspend";
      before = [ "sleep.target" ];
      wantedBy = [ "suspend.target" ];

      serviceConfig = {
        User = config.my.user.username;
        Type = "simple";
        Environment = "DISPLAY=:0";
        ExecStart = "${i3lockCommand}";
        ExecStartPost = "${pkgs.coreutils}/bin/sleep 1";
      };
    };

    # Enable autorandr for screen setups.
    services.autorandr.enable = true;

    # Enable auto locking of the screen
    services.xserver.xautolock.enable = true;
    services.xserver.xautolock.locker = "${i3lockCommand}";
    services.xserver.xautolock.enableNotifier = true;
    services.xserver.xautolock.notify = 10;
    services.xserver.xautolock.notifier = "${pkgs.libnotify}/bin/notify-send \"Locking in 10 seconds\"";
    services.xserver.xautolock.time = 3;

    # Set up services needed for gnome stuff for evolution
    services.gnome3.evolution-data-server.enable = true;
    services.gnome3.gnome-keyring.enable = true;

    # Install aditional packages
    environment.systemPackages = with pkgs; [
      evince
      gnome3.adwaita-icon-theme # Icons for gnome packages that sometimes use them but don't depend on them
      gnome3.evolution
      scrot
      i3lock-pixeled
      pavucontrol
    ];

    # Enable exwm with my emacs modules
    my.emacs.enableExwm = true;

    services.xserver.windowManager.session = singleton {
      name = "exwm";
      start = ''
        ${config.services.emacs.package}/bin/emacs -l ${myExwmInit}
      '';
    };
  };
}
