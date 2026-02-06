{ config, ... }:
{
  # Set up mako, a notification daemon for wayland
  services.mako = {
    enable = true;
    settings = {
      border-size = 3;
      default-timeout = 6000;
      font = "${config.etu.graphical.theme.fonts.monospace} ${toString config.etu.graphical.theme.fonts.size}";
    };
  };
}
