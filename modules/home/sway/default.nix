{
  config,
  osConfig,
  lib,
  pkgs,
  ...
}:
let
  lockCommand = pkgs.writeShellApplication {
    name = "lock";
    runtimeInputs = [
      pkgs.dbus
      pkgs.openssh
      config.programs.dank-material-shell.package
    ];
    text = ''
      ssh-add -D
      dbus-send --dest=org.gnome.keyring --print-reply /org/freedesktop/secrets org.freedesktop.Secret.Service.LockService || true
      dms ipc lock lock
    '';
  };
in
{
  # Set up a wallpaper manager.
  services.wpaperd.enable = true;
  services.wpaperd.settings = {
    default = {
      duration = "30m";
      mode = "center";
    };
    any.path = osConfig.etu.graphical.sway.wallpaper;
  };

  # Enable mpris-proxy to proxy bluetooth media devices onto MPRIS (dms-shell
  # controls MPRIS players directly, so playerctld is no longer needed).
  services.mpris-proxy.enable = true;

  # Configure swayidle to lock the screen on suspend and on explicit
  # loginctl lock-session calls (idle-timeout lock/suspend is handled by
  # dms-shell's own idle manager instead, see modules/home/dms-shell).
  services.swayidle.enable = true;
  services.swayidle.events.before-sleep = "${lockCommand}/bin/lock";
  services.swayidle.events.lock = "${lockCommand}/bin/lock";

  # Set up the cursor theme
  home.pointerCursor = {
    enable = true;
    name = "Adwaita";
    size = 24;
    package = pkgs.adwaita-icon-theme;
  };

  # Set up some session environment variables
  home.sessionVariables = {
    SDL_VIDEODRIVER = "wayland";

    # Firefox wayland
    MOZ_ENABLE_WAYLAND = "1";

    # Run QT programs in wayland
    QT_QPA_PLATFORM = "wayland";

    # Set the TERMINAL environment variable for apps that spawn one
    TERMINAL = osConfig.etu.graphical.terminal.terminalName;
  };

  wayland.systemd.target = "sway-session.target";

  # Sway user configs
  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;

    config =
      let
        # Set default modifier
        modifier = "Mod4";

        # Direction keys (Emacs logic)
        left = "b";
        right = "f";
        up = "p";
        down = "n";
      in
      {
        # Set default modifier
        inherit
          modifier
          left
          right
          up
          down
          ;

        keybindings = {
          # Run terminal
          "${modifier}+Return" = "exec ${osConfig.etu.graphical.terminal.terminalPath}";

          # Run Launcher
          "${modifier}+e" = "exec dms ipc launcher open";

          # Open the launcher pre-filled with the emoji/unicode launcher
          # plugin's trigger (see modules/home/dms-shell).
          "${modifier}+i" = "exec dms ipc call launcher openQuery ':e '";

          # Open the dms power menu (lock/suspend/reboot/shutdown)
          "${modifier}+Escape" = "exec dms ipc powermenu open";

          # Printscreen
          Print = "exec dms screenshot region";

          # Backlight:
          XF86MonBrightnessUp = "exec dms ipc call brightness increment 10 ''";
          XF86MonBrightnessDown = "exec dms ipc call brightness decrement 10 ''";

          # Audio:
          XF86AudioMute = "exec dms ipc call audio mute";
          XF86AudioLowerVolume = "exec dms ipc call audio decrement 10";
          XF86AudioRaiseVolume = "exec dms ipc call audio increment 10";
          XF86AudioMicMute = "exec dms ipc call mic mute";
          XF86AudioPrev = "exec dms ipc call mpris previous";
          XF86AudioPlay = "exec dms ipc call mpris playPause";
          XF86AudioNext = "exec dms ipc call mpris next";

          # Misc buttons:
          XF86Tools = "exec ${osConfig.services.emacs.package}/bin/emacs";
          XF86Favorites = "exec ${osConfig.services.emacs.package}/bin/emacs";

          # Lock the screen (caught by swayidle's events.lock, which runs lockCommand)
          "${modifier}+l" = "exec loginctl lock-session";

          # Kill focused window
          "${modifier}+Shift+apostrophe" = "kill";

          # Move focus around (emacs directions):
          "${modifier}+${left}" = "focus left";
          "${modifier}+${right}" = "focus right";
          "${modifier}+${up}" = "focus up";
          "${modifier}+${down}" = "focus down";

          # Move focus around with cursor keys:
          "${modifier}+Left" = "focus left";
          "${modifier}+Down" = "focus down";
          "${modifier}+Up" = "focus up";
          "${modifier}+Right" = "focus right";

          # Move focused window (emacs directions):
          "${modifier}+Shift+${left}" = "move left";
          "${modifier}+Shift+${right}" = "move right";
          "${modifier}+Shift+${up}" = "move up";
          "${modifier}+Shift+${down}" = "move down";

          # Move focused window with cursor keys:
          "${modifier}+Shift+Left" = "move left";
          "${modifier}+Shift+Down" = "move down";
          "${modifier}+Shift+Up" = "move up";
          "${modifier}+Shift+Right" = "move right";

          # Switch to workspace:
          "${modifier}+1" = "workspace number 1";
          "${modifier}+2" = "workspace number 2";
          "${modifier}+3" = "workspace number 3";
          "${modifier}+4" = "workspace number 4";
          "${modifier}+5" = "workspace number 5";
          "${modifier}+6" = "workspace number 6";
          "${modifier}+7" = "workspace number 7";
          "${modifier}+8" = "workspace number 8";
          "${modifier}+9" = "workspace number 9";
          "${modifier}+0" = "workspace number 10";

          # Move focused container to workspace:
          "${modifier}+Shift+1" = "move container to workspace number 1";
          "${modifier}+Shift+2" = "move container to workspace number 2";
          "${modifier}+Shift+3" = "move container to workspace number 3";
          "${modifier}+Shift+4" = "move container to workspace number 4";
          "${modifier}+Shift+5" = "move container to workspace number 5";
          "${modifier}+Shift+6" = "move container to workspace number 6";
          "${modifier}+Shift+7" = "move container to workspace number 7";
          "${modifier}+Shift+8" = "move container to workspace number 8";
          "${modifier}+Shift+9" = "move container to workspace number 9";
          "${modifier}+Shift+0" = "move container to workspace number 10";

          # Split in horizontal orientation:
          "${modifier}+h" = "split h";

          # Split in vertical orientation:
          "${modifier}+v" = "split v";

          # Change layout of focused container:
          "${modifier}+o" = "layout stacking";
          "${modifier}+comma" = "layout tabbed";
          "${modifier}+period" = "layout toggle split";

          # Fullscreen for the focused container:
          "${modifier}+u" = "fullscreen toggle";

          # Toggle the current focus between tiling and floating mode:
          "${modifier}+Shift+space" = "floating toggle";

          # Swap focus between the tiling area and the floating area:
          "${modifier}+space" = "focus mode_toggle";

          # Focus the parent container
          "${modifier}+a" = "focus parent";

          # Focus the child container
          "${modifier}+d" = "focus child";

          # Move window to scratchpad:
          "${modifier}+Shift+minus" = "move scratchpad";

          # Show scratchpad window and cycle through them:
          "${modifier}+minus" = "scratchpad show";

          # Enter other modes:
          "${modifier}+r" = "mode resize";
          "${modifier}+Shift+r" = "mode passthrough";

          # Exit Sway
          "${modifier}+Shift+e" =
            "exec ${osConfig.etu.graphical.sway.package}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' '${osConfig.etu.graphical.sway.package}/bin/swaymsg exit'";
        }
        // lib.optionalAttrs osConfig.etu.graphical.window-managers.voxtype.enable {
          # Push-to-talk: hold $mod+k to record, release to transcribe
          "--no-repeat ${modifier}+k" = "exec ${pkgs.voxtype-vulkan}/bin/voxtype record start";
          "--no-repeat --release ${modifier}+k" = "exec ${pkgs.voxtype-vulkan}/bin/voxtype record stop";
        };

        modes.resize = {
          "${left}" = "resize shrink width 10px"; # Pressing left will shrink the window's width.
          "${right}" = "resize grow width 10px"; # Pressing right will grow the window's width.
          "${up}" = "resize shrink height 10px"; # Pressing up will shrink the window's height.
          "${down}" = "resize grow height 10px"; # Pressing down will grow the window's height.

          # You can also use the arrow keys:
          Left = "resize shrink width 10px";
          Down = "resize grow height 10px";
          Up = "resize shrink height 10px";
          Right = "resize grow width 10px";

          # Exit mode
          Return = "mode default";
          Escape = "mode default";
          "${modifier}+r" = "mode default";
        };
        modes.passthrough = {
          # Exit mode
          "Shift+Escape" = "mode default";
          "${modifier}+Shift+r" = "mode default";
        };

        focus.wrapping = "workspace";
        focus.newWindow = "urgent";
        fonts = {
          names = [ osConfig.etu.graphical.theme.fonts.monospace ];
          size = osConfig.etu.graphical.theme.fonts.size + 0.0;
        };
        gaps.inner = 5;

        defaultWorkspace = "workspace number 1";

        window.commands = [
          # Set borders instead of title bars for some programs
          {
            criteria.app_id = "Alacritty";
            command = "border pixel 3";
          }
          {
            criteria.app_id = "foot";
            command = "border pixel 3";
          }
          {
            criteria.app_id = "firefox";
            command = "border pixel 3";
          }
          {
            criteria.class = "Brave-browser";
            command = "border pixel 3";
          }
          {
            criteria.class = "Chromium-browser";
            command = "border pixel 3";
          }
          {
            criteria.class = "Google-chrome";
            command = "border pixel 3";
          }
          {
            criteria.app_id = "emacs";
            command = "border pixel 3";
          }
          {
            criteria.app_id = "wlroots";
            command = "border pixel 3";
          }

          # Set opacity for some programs
          {
            criteria.app_id = "Alacritty";
            command = "opacity set 0.9";
          }
          {
            criteria.app_id = "foot";
            command = "opacity set 0.9";
          }
          {
            criteria.app_id = "emacs";
            command = "opacity set 0.99";
          }
        ];

        # Make some programs floating
        floating.criteria = [
          {
            app_id = "firefox";
            title = "Firefox - Sharing Indicator";
          }
          {
            app_id = "firefox";
            title = "Firefox — Sharing Indicator";
          }
          {
            app_id = "firefox";
            title = "Picture-in-Picture";
          }
          { title = "Welcome to Google Chrome"; }
          {
            class = "Google-chrome";
            title = "Share your new meeting - Google Chrome";
          }
          {
            app_id = "nm-connection-editor";
            title = "Network Connections";
          }
        ];

        # Set a custom keymap
        input."type:keyboard".xkb_model = osConfig.etu.graphical.xkb-keymap.model;
        input."type:keyboard".xkb_layout = osConfig.etu.graphical.xkb-keymap.layout;
        input."type:keyboard".xkb_options = osConfig.etu.graphical.xkb-keymap.options;
        input."type:keyboard".xkb_variant = osConfig.etu.graphical.xkb-keymap.variant;

        # Enable titlebars
        window.titlebar = true;
        floating.titlebar = true;

        startup = [
          # Import variables needed for screen sharing and gnome3 pinentry to work.
          { command = "${pkgs.dbus}/bin/dbus-update-activation-environment WAYLAND_DISPLAY"; }

          # Import user environment PATH to systemctl as user and then restart the xdg-desktop-portal
          # This is to get xdg-open to work in flatpaks to be able to open links inside of flatpaks.
          {
            command = "${osConfig.systemd.package}/bin/systemctl --user import-environment PATH && ${osConfig.systemd.package}/bin/systemctl --user restart xdg-desktop-portal.service";
          }
        ];

        # Disable the default bar
        bars = [ { mode = "invisible"; } ];
      };
  };
}
