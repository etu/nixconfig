{
  config,
  lib,
  ...
}:
{
  imports = [
    ./kanshi
    ./mako
    ./quickshell
    ./voxtype
    ./waybar
  ];

  config = lib.mkIf config.etu.graphical.sway.enable {
    # Set up kanshi (which kinda is an autorandr for wayland)
    etu.graphical.window-managers.kanshi.enable = true;

    # Set up mako, a notification deamon for wayland
    etu.graphical.window-managers.mako.enable = true;

    # Set up quickshell, runs alongside waybar for extra widgets
    etu.graphical.window-managers.quickshell.enable = true;

    # Set up waybar, a bar for wayland
    etu.graphical.window-managers.waybar.enable = true;
  };
}
