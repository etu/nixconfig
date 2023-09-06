{
  config,
  lib,
  ...
}: {
  imports = [
    ./mako
  ];

  config = lib.mkIf config.etu.graphical.sway.enable {
    # Set up mako, a notification deamon for wayland
    etu.graphical.window-managers.mako.enable = true;
  };
}
