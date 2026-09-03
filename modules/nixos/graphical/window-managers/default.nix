{
  config,
  lib,
  ...
}:
{
  imports = [
    ./dms-shell
    ./kanshi
    ./quickshell
    ./voxtype
  ];

  config = lib.mkIf config.etu.graphical.sway.enable {
    # Set up kanshi (which kinda is an autorandr for wayland)
    etu.graphical.window-managers.kanshi.enable = true;

    # Set up quickshell, runs alongside waybar for extra widgets
    etu.graphical.window-managers.quickshell.enable = true;

    # Set up dms-shell.
    etu.graphical.window-managers.dms-shell.enable = true;
  };
}
