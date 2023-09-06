{
  config,
  lib,
  ...
}: {
  imports = [
    ./kanshi
    ./mako
  ];

  config = lib.mkIf config.etu.graphical.sway.enable {
    # Set up kanshi (which kinda is an autorandr for wayland)
    etu.graphical.window-managers.kanshi.enable = true;

    # Set up mako, a notification deamon for wayland
    etu.graphical.window-managers.mako.enable = true;
  };
}
