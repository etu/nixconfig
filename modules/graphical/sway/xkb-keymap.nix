{ writeText, xorg }:

writeText "us-dvorak-compose" ''
  // This file defines my own custom keymap. More information about which
  // parts that gets included are available in the different subfolders in:
  // ${xorg.xkeyboardconfig}/share/X11/xkb/
  xkb_keymap {
    xkb_keycodes { include "evdev+aliases(qwerty)" };
    xkb_types    { include "default" };
    xkb_compat   { include "complete" };
    xkb_symbols  {
      include "pc+us(dvorak)+inet(evdev)+ctrl(nocaps)+eurosign(e)+kpdl(dot)"

      // Less than/Greater than/Pipe key on Swedish keyboards becomes Compose
      replace key <LSGT> { [ Multi_key ] };

      // Scroll Lock becomes Compose
      replace key <SCLK> { [ Multi_key ] };
    };
    xkb_geometry { include "pc(pc105)" };
  };
''
