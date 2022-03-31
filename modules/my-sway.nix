{ config, lib, pkgs, ... }:
let
  cfg = config.my.sway;
in
{
  config = lib.mkIf cfg.enable {
    programs.sway.enable = true;

    # Install fonts needed for waybar
    fonts.fonts = [ pkgs.font-awesome ];

    # Loginmanager
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.autoLogin.enable = true;
    services.xserver.displayManager.autoLogin.user = config.my.user.username;

    # Set up services needed for gnome stuff for evolution
    services.gnome.evolution-data-server.enable = true;
    services.gnome.gnome-keyring.enable = true;

    # Install aditional packages
    environment.systemPackages = with pkgs; [
      evince
      evolution
      gnome3.adwaita-icon-theme # Icons for gnome packages that sometimes use them but don't depend on them
      pavucontrol
      wdisplays
      wlr-randr

      # Scripts to switch between backgrounds
      (writeShellScriptBin "sway-defaultbg" ''
        ${cfg.package}/bin/swaymsg "output * bg ${cfg.wallpaperPackage}/default.jpg fill"
      '')
      (writeShellScriptBin "sway-720pfigure" ''
        ${cfg.package}/bin/swaymsg "output * bg ${cfg.wallpaperPackage}/720pfigure.jpg fill"
      '')
    ];

    # Set up Pipewire
    services.pipewire.enable = true;

    # Set up XDG Portals
    xdg.portal.enable = true;
    xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];

    # Needed for autologin
    services.xserver.displayManager.defaultSession = "sway";
  };
}
