{ lib, ... }:
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
  services.xserver.xkb.model = "pc105";
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.variant = "dvorak";
  services.xserver.xkb.options = lib.concatStringsSep "," [
    "compose:102" # Make the LSGT/"<>"/"&lt; &gt;" key a Compose key
    "compose:sclk" # Make the Scroll Lock key a Compose key
    "ctrl:nocaps" # Make the Caps Lock a Ctrl key
    "eurosign:e" # Make a Euro on E, third level
    "kpdl:dot" # Make the keypad comma key a dot
    "numpad:mac" # Numeric keypad always enters digits (as in macOS)
    "terminate:ctrl_alt_bksp" # Remove the Ctrl+Alt+Backspace behavior
  ];
}
