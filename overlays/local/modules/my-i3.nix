{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.i3;
  myUser = "etu";

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
    # Libinput
    services.xserver.libinput.enable = true;

    # Loginmanager
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.autoLogin.enable = true;
    services.xserver.displayManager.lightdm.autoLogin.user = myUser;

    # Needed for autologin
    services.xserver.desktopManager.default = "none";
    services.xserver.windowManager.default = "i3";

    # Set up i3
    services.xserver.windowManager.i3.enable = true;
    services.xserver.windowManager.i3.extraPackages = with pkgs; [
      dmenu
      i3status
      i3lock
      pavucontrol
      gnome3.networkmanagerapplet
    ];

    # Set up services needed for gnome stuff for evolution
    services.gnome3.evolution-data-server.enable = true;
    services.gnome3.gnome-keyring.enable = true;

    # Install aditional packages
    environment.systemPackages = with pkgs; [
      gnome3.evolution
    ];

    # Configure TERMINAL for i3-sensible-terminal
    environment.variables.TERMINAL = "stupidterm";

    # Enable i3lock on suspend
    systemd.services.i3lock = {
      description = "Lock screen before suspend";
      before = [ "sleep.target" ];
      wantedBy = [ "suspend.target" ];

      serviceConfig = {
        User = myUser;
        Type = "simple";
        Environment = "DISPLAY=:0";
        ExecStart = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
        ExecStartPost = "${pkgs.coreutils}/bin/sleep 1";
      };
    };
  };
}
