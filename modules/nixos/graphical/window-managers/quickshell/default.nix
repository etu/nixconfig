{
  config,
  lib,
  flake,
  ...
}:
{
  options.etu.graphical.window-managers.quickshell.enable =
    lib.mkEnableOption "Enable quickshell, a QtQuick-based desktop shell toolkit";

  config = lib.mkIf config.etu.graphical.window-managers.quickshell.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.quickshell
      ];
    };
  };
}
