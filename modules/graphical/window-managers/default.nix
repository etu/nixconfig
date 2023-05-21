{
  config,
  lib,
  ...
}: {
  imports = [
    ./kanshi
    ./mako
    ./waybar
  ];

  config = lib.mkIf (config.etu.graphical.sway.enable || config.etu.graphical.hyprland.enable) {
    # Set up kanshi (which kinda is an autorandr for wayland)
    etu.graphical.window-managers.kanshi.enable = true;

    # Set up mako, a notification deamon for wayland
    etu.graphical.window-managers.mako.enable = true;

    # Set up waybar, a bar for wayland
    etu.graphical.window-managers.waybar.enable = true;
  };
}
