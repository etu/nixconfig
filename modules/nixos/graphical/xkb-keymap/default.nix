{lib, ...}: {
  options.etu.graphical.xkb-keymap.model = lib.mkOption {
    type = lib.types.str;
    default = "pc105";
    description = "Physical keyboard of use";
  };

  options.etu.graphical.xkb-keymap.layout = lib.mkOption {
    type = lib.types.str;
    default = "us";
    description = "Keyboard layout to use";
  };

  options.etu.graphical.xkb-keymap.variant = lib.mkOption {
    type = lib.types.str;
    default = "dvorak";
    description = "Keyboard variant to use";
  };

  options.etu.graphical.xkb-keymap.options = lib.mkOption {
    type = lib.types.commas;
    default = lib.strings.concatStringsSep "," [
      "compose:102" # Make the LSGT/"<>"/"&lt; &gt;" key a Compose key
      "compose:sclk" # Make the Scroll Lock key a Compose key
      "ctrl:nocaps" # Make the Caps Lock a Ctrl key
      "eurosign:e" # Make a Euro on E, third level
      "kpdl:dot" # Make the keypad comma key a dot
      "numpad:mac" # Numeric keypad always enters digits (as in macOS)
      "terminate:ctrl_alt_bksp" # Remove the Ctrl+Alt+Backspace behavior
    ];
    description = "Additional options to include for layout";
  };
}
