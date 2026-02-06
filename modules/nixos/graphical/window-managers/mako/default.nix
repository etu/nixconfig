{
  config,
  lib,
  flake,
  ...
}:
{
  options.etu.graphical.window-managers.mako.enable =
    lib.mkEnableOption "Enable mako, a notification daemon for wayland";

  config = lib.mkIf config.etu.graphical.window-managers.mako.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.mako
      ];
    };
  };
}
