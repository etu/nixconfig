{ config, lib, pkgs, ... }:
let
  cfg = config.my.home-manager;
  swayCfg = config.my.sway;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.user.username} = {
      # Configure swayidle for automatic screen locking
      services.swayidle = {
        enable = swayCfg.enable;
        timeouts = [
          { timeout = 300; command = swayCfg.lockCommand; }
          { timeout = 600; command = "${swayCfg.package}/bin/swaymsg 'output * dpms off'"; }
        ];
        events = [
          { event = "after-resume"; command = "${swayCfg.package}/bin/swaymsg 'output * dpms on'"; }
          { event = "before-sleep"; command = swayCfg.lockCommand; }
        ];
      }; # END swayidle

      # Set up mako, a notification deamon for wayland
      programs.mako = {
        enable = swayCfg.enable;
        backgroundColor = "#191311";
        borderColor = "#3B7C87";
        borderSize = 3;
        defaultTimeout = 6000;
        font = "${config.my.fonts.monospace} ${toString (builtins.floor config.my.fonts.size)}";
      }; # END mako

      # Set up kanshi (which kinda is an autorandr for wayland)
      services.kanshi = {
        enable = swayCfg.enable;
        profiles = {
          undocked.outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
            }
          ];
          docked.outputs = [
            { criteria = "Samsung Electric Company LC49G95T H4ZN900853 (DP-1)";
              mode = "5120x1440"; }
            { criteria = "eDP-1";
              status = "disable"; }
          ];
        };
      }; # END kanshi

      # Sway user configs
      wayland.windowManager.sway = {
        enable = swayCfg.enable;
        extraSessionCommands = ''
          export SDL_VIDEODRIVER=wayland

          # Firefox wayland
          export MOZ_ENABLE_WAYLAND=1

          # XDG portal related variables (for screen sharing etc)
          export XDG_SESSION_TYPE=wayland
          export XDG_CURRENT_DESKTOP=sway

          # Run QT programs in wayland
          export QT_QPA_PLATFORM=wayland
        '';

        # TODO:
        # - Network manager applet
        # - Media keys (Missing: XF86Display)
        config = let
          rofi = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
          pactl = "${config.hardware.pulseaudio.package}/bin/pactl";

          # Set default modifier
          modifier = "Mod4";

          # Direction keys (Emacs logic)
          left = "b";
          right = "f";
          up = "p";
          down = "n";
        in {
          # Set default modifier
          inherit modifier left right up down;

          keybindings = {
            # Run terminal
            "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";

            # Run Launcher
            "${modifier}+e" = "exec ${rofi}/bin/rofi -show combi -theme glue_pro_blue | xargs swaymsg exec --";

            # Run rofi
            "${modifier}+i" = "exec ${rofi}/bin/rofi -show emoji -theme glue_pro_blue";

            # Printscreen
            Print = "exec ${pkgs.grim}/bin/grim -g \"$(${pkgs.slurp}/bin/slurp)\" - | ${pkgs.swappy}/bin/swappy -f -";

            # Backlight:
            XF86MonBrightnessUp = "exec ${pkgs.acpilight}/bin/xbacklight -inc 10";
            XF86MonBrightnessDown = "exec ${pkgs.acpilight}/bin/xbacklight -dec 10";

            # Audio:
            XF86AudioMute = "exec ${pactl} set-sink-mute @DEFAULT_SINK@ toggle";
            XF86AudioLowerVolume = "exec ${pactl} set-sink-volume @DEFAULT_SINK@ -10%";
            XF86AudioRaiseVolume = "exec ${pactl} set-sink-volume @DEFAULT_SINK@ +10%";
            XF86AudioMicMute = "exec ${pactl} set-source-mute @DEFAULT_SOURCE@ toggle";

            # Misc buttons:
            XF86Tools = "exec ${config.services.emacs.package}/bin/emacs";
            XF86Favorites = "exec ${config.services.emacs.package}/bin/emacs";

            # Launch screen locker
            "${modifier}+l" = "exec ${swayCfg.lockCommand}";

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
            "${modifier}+Shift+e" = "exec ${swayCfg.package}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' '${swayCfg.package}/bin/swaymsg exit'";
          } // lib.optionalAttrs config.my.gaming.enable {
            # Add PTT button for mumble in the gaming module:
            "--no-repeat Alt_R" = "exec ${pkgs.glib}/bin/gdbus call -e -d net.sourceforge.mumble.mumble -o / -m net.sourceforge.mumble.Mumble.startTalking";
            "--no-repeat --release Alt_R" = "exec ${pkgs.glib}/bin/gdbus call -e -d net.sourceforge.mumble.mumble -o / -m net.sourceforge.mumble.Mumble.stopTalking";
          };

          modes.resize = {
            "${left}" = "resize shrink width 10px"; # Pressing left will shrink the window’s width.
            "${right}" = "resize grow width 10px";  # Pressing right will grow the window’s width.
            "${up}" = "resize shrink height 10px";  # Pressing up will shrink the window’s height.
            "${down}" = "resize grow height 10px";  # Pressing down will grow the window’s height.

            # You can also use the arrow keys:
            Left = "resize shrink width 10 px";
            Down = "resize grow height 10 px";
            Up = "resize shrink height 10 px";
            Right = "resize grow width 10 px";

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

          focus.forceWrapping = true;
          fonts = {
            names = [ config.my.fonts.monospace ];
            inherit (config.my.fonts) size;
          };
          gaps.inner = 5;

          defaultWorkspace = "workspace number 1";

          window.commands = [
            # Set borders instead of title bars for some programs
            { criteria.app_id = "Alacritty"; command = "border pixel 3"; }
            { criteria.app_id = "firefox"; command = "border pixel 3"; }
            { criteria.class = "Brave-browser"; command = "border pixel 3"; }
            { criteria.class = "Chromium-browser"; command = "border pixel 3"; }
            { criteria.class = "Google-chrome"; command = "border pixel 3"; }
            { criteria.class = "Emacs"; command = "border pixel 3"; }
            { criteria.app_id = "emacs"; command = "border pixel 3"; }
            { criteria.app_id = "wlroots"; command = "border pixel 3"; }

            # Set opacity for some programs
            { criteria.app_id = "Alacritty"; command = "opacity set 0.9"; }
            { criteria.class = "Emacs"; command = "opacity set 0.99"; }
            { criteria.app_id = "emacs"; command = "opacity set 0.99"; }
          ];

          # Make some programs floating
          floating.criteria = [
            { app_id = "firefox"; title = "Firefox - Sharing Indicator"; }
            { app_id = "firefox"; title = "Firefox — Sharing Indicator"; }
            { app_id = "firefox"; title = "Picture-in-Picture"; }
            { title = "Welcome to Google Chrome"; }
          ];

          # Set a custom keymap
          input."type:keyboard".xkb_file = toString swayCfg.xkbFile;

          # Set wallpaper for all outputs
          output."*".bg = "${swayCfg.wallpaperPackage}/default.jpg fill";

          # Enable titlebars
          window.titlebar = true;
          floating.titlebar = true;

          startup = [
            { command = "${pkgs.mako}/bin/mako"; }

            # Import variables needed for screen sharing and gnome3 pinentry to work.
            { command = "${pkgs.dbus}/bin/dbus-update-activation-environment WAYLAND_DISPLAY"; }
          ];

          # Disable the default bar
          bars = [{ mode = "invisible"; }];
        };
      }; # END sway

      # Configure waybar
      programs.waybar = {
        enable = swayCfg.enable;
        systemd.enable = swayCfg.enable;
        systemd.target = "sway-session.target";
        style = pkgs.runCommandNoCC "waybar-styles.css" { } ''
          sed -e 's/font-family: /font-family: ${config.my.fonts.normal}, /'              \
              -e 's/font-size: 13px/font-size: ${toString (builtins.floor config.my.fonts.biggerSize)}px/' \
              ${pkgs.waybar}/etc/xdg/waybar/style.css > $out
        '';
        settings = [{
          # Height of bar
          height = 30;

          # Margins for bar
          margin-top = 0;
          margin-bottom = 0;
          margin-right = 100;
          margin-left = 100;

          modules-left = [ "idle_inhibitor" "backlight" "cpu" "memory" "temperature" "battery" "battery#bat2" ];
          modules-center = [ "sway/workspaces" "sway/mode" ];
          modules-right = [ "pulseaudio" "network" "clock" "tray" ];

          "sway/workspaces" = {
            disable-scroll = true;
            format = "{icon}";
            format-icons = {
              "1" = ""; # Ⅰ
              "2" = ""; # Ⅱ
              "3" = ""; # Ⅲ
              "4" = "Ⅳ";
              "5" = "Ⅴ";
              "6" = "Ⅵ";
              "7" = "Ⅶ";
              "8" = "Ⅷ";
              "9" = "Ⅸ";
              "10" = "Ⅹ";
              urgent = "";
              focused = "";
              default = "";
            };
          };

          "sway/mode".format = "<span style=\"italic\">{}</span>";

          "battery#bat2".bat = "BAT2";

          backlight.format = "{percent}% {icon}";
          backlight.format-icons = [ "" "" ];

          battery.format = "{capacity}% {icon}";
          battery.format-alt = "{time} {icon}";
          battery.format-charging = "{capacity}% ";
          battery.format-full = "{capacity}% {icon}";
          battery.format-good = "{capacity}% {icon}";
          battery.format-icons = [ "" "" "" "" "" ];
          battery.format-plugged = "{capacity}% ";
          battery.states = { good = 80; warning = 30; critical = 15; };

          clock.format = "<span color=\"#88c0d0\"></span> {:%Y-%m-%d %H:%M:%S}";
          clock.interval = 5;

          cpu.format = "{usage}% ";
          cpu.tooltip = true;

          idle_inhibitor.format = "{icon}";
          idle_inhibitor.format-icons.activated = "";
          idle_inhibitor.format-icons.deactivated = "";

          memory.format = "{}% ";

          network.format-alt = "{ifname}: {ipaddr}/{cidr}";
          network.format-disconnected = "Disconnected ⚠";
          network.format-ethernet = "{ifname}: {ipaddr}/{cidr} ";
          network.format-linked = "{ifname} (No IP) ";
          network.format-wifi = "{essid} ({signalStrength}%) ";
          network.interval = 15;

          pulseaudio.format = "{volume}% {icon} {format_source}";
          pulseaudio.format-bluetooth = "{volume}% {icon} {format_source}";
          pulseaudio.format-bluetooth-muted = " {icon} {format_source}";
          pulseaudio.format-muted = " {format_source}";
          pulseaudio.format-source = "{volume}% ";
          pulseaudio.format-source-muted = "";
          pulseaudio.format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = [ "" "" "" ];
          };
          pulseaudio.on-click = "${pkgs.pavucontrol}/bin/pavucontrol";

          temperature.critical-threshold = 80;
          temperature.format = "{icon} {temperatureC}°C";
          temperature.format-icons = [ "" "" "" ];
        }];
      }; # END waybar
    }; # END home-manager
  }; # END config
}
