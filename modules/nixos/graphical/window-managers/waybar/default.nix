{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
{
  options.etu.graphical.window-managers.waybar.enable =
    lib.mkEnableOption "Enable waybar, a bar for wayland";

  config = lib.mkIf config.etu.graphical.window-managers.waybar.enable {
    # Install fonts needed for waybar
    fonts.packages = [ pkgs.font-awesome ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.waybar
      ];
    };
  };
}
