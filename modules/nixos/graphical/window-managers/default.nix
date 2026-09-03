{
  config,
  lib,
  ...
}:
{
  imports = [
    ./dms-shell
    ./kanshi
    ./voxtype
  ];

  config = lib.mkIf config.etu.graphical.sway.enable {
    # Set up kanshi (which kinda is an autorandr for wayland)
    etu.graphical.window-managers.kanshi.enable = true;

    # Set up dms-shell.
    etu.graphical.window-managers.dms-shell.enable = true;
  };
}
