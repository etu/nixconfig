{
  config,
  lib,
  ...
}:
let
  # Mirror modules/home/sway: Alt instead of Super (Cmd collides with most
  # macOS shortcuts), and Emacs-style direction keys (b/f/p/n).
  modifier = "alt";

  # Start a new Ghostty instance, like sway's exec of the terminal. AppleScript
  # ("tell application Ghostty to new window") doesn't work here: AeroSpace
  # doesn't declare NSAppleEventsUsageDescription, so macOS silently refuses
  # Apple Events sent on its behalf. ghostty +new-window isn't supported on
  # macOS either.
  newTerminal = "open -na Ghostty";
in
{
  options.etu.graphical.aerospace.enable =
    lib.mkEnableOption "Enable the AeroSpace tiling window manager";

  config = lib.mkIf config.etu.graphical.aerospace.enable {
    services.aerospace.enable = true;

    services.aerospace.settings = {
      config-version = 2;

      # Bind keys by Dvorak letters, like on sway.
      key-mapping.preset = "dvorak";

      # Workspaces I-X always exist, like sway's numbered workspaces (bound to
      # keys 1-9 and 0 below).
      persistent-workspaces = [
        "Ⅰ"
        "Ⅱ"
        "Ⅲ"
        "Ⅳ"
        "Ⅴ"
        "Ⅵ"
        "Ⅶ"
        "Ⅷ"
        "Ⅸ"
        "Ⅹ"
      ];

      gaps.inner.horizontal = 5;
      gaps.inner.vertical = 5;

      mode.main.binding = {
        # Run terminal
        "${modifier}-enter" = "exec-and-forget ${newTerminal}";

        # Kill focused window
        "${modifier}-shift-quote" = "close";

        # Split in horizontal/vertical orientation (sway's split h/v).
        "${modifier}-h" = "layout horizontal";
        "${modifier}-v" = "layout vertical";

        # Change layout of focused container (stacking, tabbed, toggle split).
        "${modifier}-o" = "layout v_accordion";
        "${modifier}-comma" = "layout h_accordion";
        "${modifier}-period" = "layout tiles horizontal vertical";

        # Fullscreen for the focused container.
        "${modifier}-u" = "fullscreen";

        # Toggle the focused window between tiling and floating.
        "${modifier}-shift-space" = "layout floating tiling";

        # Enter resize mode.
        "${modifier}-r" = "mode resize";

        # Move focus around (emacs directions), wrapping within the workspace
        # like sway's focus.wrapping = "workspace":
        "${modifier}-b" = "focus --wrap-around left";
        "${modifier}-f" = "focus --wrap-around right";
        "${modifier}-p" = "focus --wrap-around up";
        "${modifier}-n" = "focus --wrap-around down";

        # Move focus around with cursor keys:
        "${modifier}-left" = "focus --wrap-around left";
        "${modifier}-right" = "focus --wrap-around right";
        "${modifier}-up" = "focus --wrap-around up";
        "${modifier}-down" = "focus --wrap-around down";

        # Move focused window (emacs directions):
        "${modifier}-shift-b" = "move left";
        "${modifier}-shift-f" = "move right";
        "${modifier}-shift-p" = "move up";
        "${modifier}-shift-n" = "move down";

        # Move focused window with cursor keys:
        "${modifier}-shift-left" = "move left";
        "${modifier}-shift-right" = "move right";
        "${modifier}-shift-up" = "move up";
        "${modifier}-shift-down" = "move down";

        # Switch to workspace:
        "${modifier}-1" = "workspace Ⅰ";
        "${modifier}-2" = "workspace Ⅱ";
        "${modifier}-3" = "workspace Ⅲ";
        "${modifier}-4" = "workspace Ⅳ";
        "${modifier}-5" = "workspace Ⅴ";
        "${modifier}-6" = "workspace Ⅵ";
        "${modifier}-7" = "workspace Ⅶ";
        "${modifier}-8" = "workspace Ⅷ";
        "${modifier}-9" = "workspace Ⅸ";
        "${modifier}-0" = "workspace Ⅹ";

        # Move focused window to workspace:
        "${modifier}-shift-1" = "move-node-to-workspace Ⅰ";
        "${modifier}-shift-2" = "move-node-to-workspace Ⅱ";
        "${modifier}-shift-3" = "move-node-to-workspace Ⅲ";
        "${modifier}-shift-4" = "move-node-to-workspace Ⅳ";
        "${modifier}-shift-5" = "move-node-to-workspace Ⅴ";
        "${modifier}-shift-6" = "move-node-to-workspace Ⅵ";
        "${modifier}-shift-7" = "move-node-to-workspace Ⅶ";
        "${modifier}-shift-8" = "move-node-to-workspace Ⅷ";
        "${modifier}-shift-9" = "move-node-to-workspace Ⅸ";
        "${modifier}-shift-0" = "move-node-to-workspace Ⅹ";
      };

      mode.resize.binding = {
        # Resize the focused window (emacs directions):
        b = "resize width -50"; # Pressing left will shrink the window's width.
        f = "resize width +50"; # Pressing right will grow the window's width.
        p = "resize height -50"; # Pressing up will shrink the window's height.
        n = "resize height +50"; # Pressing down will grow the window's height.

        # You can also use the arrow keys:
        left = "resize width -50";
        right = "resize width +50";
        up = "resize height -50";
        down = "resize height +50";

        # Exit mode
        enter = "mode main";
        esc = "mode main";
        "${modifier}-r" = "mode main";
      };
    };
  };
}
