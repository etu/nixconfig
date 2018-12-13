{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.i3;

in {
  options = {
    my.i3 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enables i3wm and auto login for my user
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.xserver.libinput.enable = true;

    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.autoLogin.enable = true;
    services.xserver.displayManager.lightdm.autoLogin.user = "etu";

    services.xserver.desktopManager.default = "none";
    services.xserver.windowManager.default = "i3";

    services.xserver.windowManager.i3.enable = true;
    services.xserver.windowManager.i3.extraPackages = with pkgs; [
      dmenu
      i3status
      i3lock
      rxvt_unicode
      pavucontrol
      gnome3.networkmanagerapplet
    ];

    environment.variables.TERMINAL = "stupidterm";
  };
}
