{
  config,
  lib,
  ...
}:
{
  imports = [
    ./kanshi
    ./mako
    ./waybar
  ];

  config = lib.mkIf config.etu.graphical.sway.enable {
    # Set up kanshi (which kinda is an autorandr for wayland)
    etu.graphical.window-managers.kanshi.enable = true;

    # Set up mako, a notification deamon for wayland
    etu.graphical.window-managers.mako.enable = true;

    # Set up waybar, a bar for wayland
    etu.graphical.window-managers.waybar.enable = true;

    # Disable the NixOS-managed blueman applet so home-manager can own it;
    # withApplet = true (default) generates a drop-in that conflicts with the
    # package-provided unit, producing two ExecStart= directives.
    services.blueman.withApplet = lib.mkIf config.services.blueman.enable false;
  };
}
