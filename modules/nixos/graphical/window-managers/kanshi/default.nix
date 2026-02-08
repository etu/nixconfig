{
  config,
  lib,
  flake,
  ...
}:
{
  options.etu.graphical.window-managers.kanshi.enable =
    lib.mkEnableOption "Enable kanshi, kinda autorandr for wayland";

  config = lib.mkIf config.etu.graphical.window-managers.kanshi.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.kanshi
      ];
    };
  };
}
