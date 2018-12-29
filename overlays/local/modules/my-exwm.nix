{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.exwm;
  loadScript = pkgs.writeText "emacs-exwm-load" ''
    (require 'exwm)
    (require 'exwm-config)

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)
    (setq exwm-systemtray-height 16)

    (exwm-config-default)
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

    # Set up services needed for gnome stuff for evolution
    services.gnome3.evolution-data-server.enable = true;
    services.gnome3.gnome-keyring.enable = true;

    # Install aditional packages
    environment.systemPackages = with pkgs; [
      evince
      gnome3.evolution
      scrot
      pavucontrol
      gnome3.networkmanagerapplet
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
