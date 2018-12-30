{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.exwm;
  loadScript = pkgs.writeText "emacs-exwm-load" ''
    (require 'exwm)
    (require 'exwm-config)
    (require 'exwm-randr)
    (require 'exwm-systemtray)

    ;; Set up systray
    (exwm-systemtray-enable)
    ;; (setq exwm-systemtray-height 32)

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
        (let ((cmd (concat "systemd-run --user " command)))
          (start-process-shell-command cmd nil cmd)))
      (define-key exwm-mode-map (kbd "s-e") 'exwm-run)
      (global-set-key (kbd "s-e") 'exwm-run)

      ;; Special function to lock the screen
      (defun exwm-run-i3lock ()
        (interactive)
        (exwm-run "${pkgs.i3lock}/bin/i3lock -c 000000"))
      (define-key exwm-mode-map (kbd "s-l") 'exwm-run-i3lock)
      (global-set-key (kbd "s-l") 'exwm-run-i3lock)

      ;; Special function to run the terminal
      (defun exwm-run-stupidterm ()
        (interactive)
        (exwm-run "stupidterm")))

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
        ExecStart = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
        ExecStartPost = "${pkgs.coreutils}/bin/sleep 1";
      };
    };

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
