{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.exwm;
  i3lockCommand = "${pkgs.i3lock}/bin/i3lock --nofork --color=000000";
  loadScript = pkgs.writeText "emacs-exwm-load" ''
    (require 'exwm)
    (require 'exwm-config)

    ;; Display time in modeline
    (progn
      (setq display-time-24hr-format t)
      (display-time-mode 1))

    ;; Display battery mode
    (display-battery-mode)

    ;; Define a function to easily run shell commands
    (progn
      (defun exwm-run (command)
        (interactive (list (read-shell-command "$ ")))
        (let ((cmd (concat "${pkgs.systemd}/bin/systemd-run --user " command)))
          (start-process-shell-command cmd nil cmd)))
      (exwm-input-set-key (kbd "s-e") 'exwm-run)

      ;; Special function to run the terminal
      (defun exwm-run-stupidterm ()
        (interactive)
        (exwm-run "${pkgs.stupidterm}/bin/stupidterm"))
      (exwm-input-set-key (kbd "s-t") 'exwm-run-stupidterm))

    ;; Define desktop environment commands
    (use-package desktop-environment
      :defer 1
      :init
      (setq desktop-environment-screenlock-command "${i3lockCommand}")
      (setq desktop-environment-screenshot-directory "~"
            desktop-environment-screenshot-command "${pkgs.flameshot}/bin/flameshot gui"
            desktop-environment-screenshot-partial-command "${pkgs.flameshot}/bin/flameshot gui")
      (setq desktop-environment-brightness-get-command "${pkgs.xorg.xbacklight}/bin/xbacklight"
            desktop-environment-brightness-set-command "${pkgs.xorg.xbacklight}/bin/xbacklight %s"
            desktop-environment-brightness-get-regexp "\\([0-9]+\\)"
            desktop-environment-brightness-normal-increment "-inc 10"
            desktop-environment-brightness-normal-decrement "-dec 10"
            desktop-environment-brightness-small-increment "-inc 5"
            desktop-environment-brightness-small-decrement "-dec 5")
      :config
      (desktop-environment-mode))

    ;; Set up systray
    (use-package exwm-systemtray
      :config
      (exwm-systemtray-enable))

    ;; Set up randr
    (use-package exwm-randr
      :config
      (setq exwm-randr-workspace-monitor-plist '(1 "eDP1" 9 "HDMI1"))
      (exwm-randr-enable))

    ;; Load exwm
    (exwm-config-default)
    (exwm-randr-enable)
  '';

in {
  options = {
    my.exwm = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enables exwm and auto login for my user
        '';
      };
    };
  };

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
      i3lock
      pavucontrol
    ];

    # Enable exwm with my emacs modules
    my.emacs.enableExwm = true;

    services.xserver.windowManager.session = singleton {
      name = "exwm";
      start = ''
        ${config.services.emacs.package}/bin/emacs -l ${loadScript}
      '';
    };
  };
}
