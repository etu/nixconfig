{
  config,
  perSystem,
  lib,
  pkgs,
  flake,
  ...
}:
{
  options.etu.graphical.sway = {
    enable = lib.mkEnableOption "Enables sway and auto login for my user";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.sway;
      description = "Which sway package to use";
      readOnly = true;
    };
    wallpaper = lib.mkOption {
      type = lib.types.str;
      default = builtins.toString perSystem.self.spaceWallpapers;
      description = "Wallpaper to use for sway";
    };
    lockWallpaper = lib.mkOption {
      type = lib.types.str;
      default = "screenshot";
      description = "Wallpaper to use for lockscreen";
    };
    enableSuspendOnTimeout = lib.mkEnableOption "Lock the screen before suspending" // {
      default = true;
    };
  };

  config = lib.mkIf config.etu.graphical.sway.enable {
    # Install packages using home manager.
    etu.user.extraUserPackages = [
      pkgs.bluetuith
      pkgs.evince
      pkgs.adwaita-icon-theme # Icons for gnome packages that sometimes use them but don't depend on them
      pkgs.pavucontrol
      pkgs.wdisplays
      pkgs.wlr-randr

      # Script to reload environment variables (if used nested sway
      # session and want chrome screen sharing to read the inner sway)
      (pkgs.writeShellScriptBin "sway-reload-env" ''
        ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd WAYLAND_DISPLAY SWAYSOCK
        ${pkgs.systemd}/bin/systemctl --user restart xdg-desktop-portal.service
        ${pkgs.systemd}/bin/systemctl --user restart xdg-desktop-portal-wlr.service
      '')
    ];

    programs.sway.enable = true;

    # Make sure to start the home-manager activation before I log in.
    systemd.services."home-manager-${config.etu.user.username}" = {
      before = [ "display-manager.service" ];
      wantedBy = [ "multi-user.target" ];
    };

    # Enable greetd as a non-graphical login manager.
    services.greetd.enable = true;
    services.greetd.settings = {
      default_session.command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd sway";
      default_session.user = "greeter";
      initial_session.command = "sway";
      initial_session.user = config.etu.user.username;
    };

    # Set up Pipewire
    services.pipewire.enable = true;
    services.pipewire.alsa.enable = true;
    services.pipewire.pulse.enable = true;
    services.pipewire.jack.enable = true;

    # Set up upower to be able to get battery levels of connected devices.
    services.upower.enable = true;

    # Set up XDG Portals
    xdg.portal.enable = true;
    xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.sway
      ];
    };
  };
}
