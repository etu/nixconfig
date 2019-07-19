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

in {
  options.my.desktop-exwm.enable = mkEnableOption "Enables exwm and auto login for my user";

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/adisbladis/exwm-overlay/archive/master.tar.gz;
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
