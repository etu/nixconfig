{ config, ... }:
{
  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "all"
    ];
  };

  # Set console font and keymap.
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;

  # Keyboard layout used by the console, X11 and other parts.
  services.xserver.xkb.model = config.etu.graphical.xkb-keymap.model;
  services.xserver.xkb.layout = config.etu.graphical.xkb-keymap.layout;
  services.xserver.xkb.options = config.etu.graphical.xkb-keymap.options;
  services.xserver.xkb.variant = config.etu.graphical.xkb-keymap.variant;
}
