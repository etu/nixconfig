{ config, lib, pkgs, ... }:
let
  cfg = config.my.home-manager;

  isX11 = config.my.emacs.enableExwm;
  isWayland = config.my.sway.enable;
  isGraphical = isX11 || isWayland;

  # Load sources
  sources = import ../nix/sources.nix;
in
{
  # Import the home-manager module
  imports = [ "${sources.home-manager}/nixos" ];

  config = lib.mkIf cfg.enable {
    # Make sure to start the home-manager activation before I log it.
    systemd.services."home-manager-${config.my.user.username}" = {
      before = [ "display-manager.service" ];
      wantedBy = [ "multi-user.target" ];
    };

    home-manager.users.${config.my.user.username} = {
        # Import a persistance module for home-manager.
        imports = [
          ./home-manager/weechat.nix
        ];

        programs.home-manager.enable = true;

        home.file = {
          # Home nix config.
          ".config/nixpkgs/config.nix".text = "{ allowUnfree = true; }";

          # Nano config
          ".nanorc".text = "set constantshow # Show linenumbers -c as default";

          # Tmux config
          ".tmux.conf".source = ./dotfiles/tmux.conf;

          # Fish config
          ".config/fish/config.fish".source = ./dotfiles/fish/config.fish;

          # Fish functions
          ".config/fish/functions".source = ./dotfiles/fish/functions;

          # Lorrirc
          ".direnvrc".text = ''
            use_nix() {
              eval "$(lorri direnv)"
            }
          '';

          # Some extra scripts
          "bin/git-branchclean".source = ./dotfiles/bin/git-branchclean;
          "bin/git-git".source = ./dotfiles/bin/git-git;
          "bin/git-lol".source = ./dotfiles/bin/git-lol;
          "bin/git-refetch-tags".source = ./dotfiles/bin/git-refetch-tags;
          "bin/restow".source = ./dotfiles/bin/restow;
          "bin/spacecolors".source = ./dotfiles/bin/spacecolors;

          "bin/keep".source = pkgs.runCommandNoCC "keep" { } ''
            cp ${./dotfiles/bin/keep} $out
            substituteInPlace $out --replace /bin/zsh ${pkgs.zsh}/bin/zsh
          '';
        } // lib.optionalAttrs config.my.emacs.enable {
          # Emacs inhibit startup screen
          ".emacs".text = "(custom-set-variables '(inhibit-startup-screen t))";
        } // lib.optionalAttrs isGraphical {
          # Mpv config file - Don't show images embedded in music files
          ".config/mpv/mpv.conf".text = "no-audio-display";

          # .XCompose
          ".XCompose".text = ''
            include "%L"

            # Default already
            # <Multi_key> <a> <a>: "å"
            # <Multi_key> <A> <A>: "Å"

            # Some nice binds
            <Multi_key> <a> <e>: "ä"
            <Multi_key> <A> <E>: "Ä"
            <Multi_key> <o> <e>: "ö"
            <Multi_key> <O> <E>: "Ö"

            # Table flip multi key
            <Multi_key> <t> <f>: "(ノಠ益ಠ)ノ彡┻━┻"

            # Shruggie
            <Multi_key> <s> <h>: "¯\\_(ツ)_/¯"
          '';
        };

        programs.alacritty.enable = isGraphical;
        programs.alacritty.settings = {
          env.TERM = "xterm-256color";
          font.size = config.my.fonts.size;
          font.normal.family = config.my.fonts.monospace;
          bell = {
            duration = 250;
            color = "#441111";
            animation = "EaseOut";
          };
          colors = {
            primary = { background = "#000000"; foreground = "#dddddd"; };
            normal = {
              black = "#000000";
              red = "#cc0403";
              green = "#19cb00";
              yellow = "#cecb00";
              blue = "#0d73cc";
              magenta = "#cb1ed1";
              cyan = "#0dcdcd";
              white = "#dddddd";
            };
            bright = {
              black = "#767676";
              red = "#f2201f";
              green = "#23fd00";
              yellow = "#fffd00";
              blue = "#1a8fff";
              magenta = "#fd28ff";
              cyan = "#14ffff";
              white = "#ffffff";
            };
          };
        };

        programs.git = {
          enable = true;

          # Default configs
          extraConfig = {
            commit.gpgSign = isGraphical;

            user.name = config.my.user.realname;
            user.email = config.my.user.email;
            user.signingKey = config.my.user.signingKey;

            # Set default "git pull" behaviour so it doesn't try to default to
            # either "git fetch; git merge" (default) or "git fetch; git rebase".
            pull.ff = "only";
          };

          # Global ignores
          ignores = [ ".ac-php-conf.json" ];

          # Conditonally included configs
          includes = [{
            condition = "gitdir:/home/${config.my.user.username}/tvnu/";
            contents = {
              commit.gpgSign = false;
              user.email = config.my.user.workEmail;
              user.signingKey = "";
            };
          }];
        };

        programs.browserpass.enable = isGraphical;

        xdg.mimeApps = {
          enable = isGraphical;
          defaultApplications = {
            "text/html" = [ "firefox.desktop" ];
            "x-scheme-handler/http" = [ "firefox.desktop" ];
            "x-scheme-handler/https" = [ "firefox.desktop" ];
            "x-scheme-handler/about" = [ "firefox.desktop" ];
            "x-scheme-handler/unknown" = [ "firefox.desktop" ];
            "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
          };

          associations.added = {
            "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
          };
        };

        # Htop configurations
        programs.htop = {
          enable = true;
          settings = {
            hide_userland_threads = true;
            hide_kernel_threads = true;
            highlight_base_name = true;
            shadow_other_users = true;
            show_program_path = true;
            tree_view = true;

            left_meters = [ "LeftCPUs" "Memory" "Swap" "ZFSARC" "ZFSCARC" ];
            left_meter_modes = [ 1 1 1 2 2 ];

            right_meters = [ "RightCPUs" "Tasks" "LoadAverage" "Uptime" "Battery" ];
            right_meter_modes = [ 1 2 2 2 2 ];
          };
        };

        # GTK theme configs
        gtk.enable = isGraphical;
        gtk.font.name = config.my.fonts.normal;
        gtk.font.size = builtins.floor config.my.fonts.size;
        gtk.gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;

        # Set up qt theme as well
        qt = {
          enable = isGraphical;
          platformTheme = "gtk";
        };

        # Set the rofi font
        programs.rofi.font = "${config.my.fonts.monospace} ${toString (builtins.floor config.my.fonts.size)}";

        # Enable syncthing.
        services.syncthing.enable = isGraphical;

        # Enable the dunst notification deamon
        services.dunst.enable = isX11;
        services.dunst.settings = {
          global = {
            # font = "";

            # Allow a small subset of html markup
            markup = "yes";
            plain_text = "no";

            # The format of the message
            format = "<b>%s</b>\\n%b";

            # Alignment of message text
            alignment = "center";

            # Split notifications into multiple lines
            word_wrap = "yes";

            # Ignore newlines '\n' in notifications.
            ignore_newline = "no";

            # Hide duplicate's count and stack them
            stack_duplicates = "yes";
            hide_duplicates_count = "yes";

            # The geometry of the window
            geometry = "420x50-15+49";

            # Shrink window if it's smaller than the width
            shrink = "no";

            # Don't remove messages, if the user is idle
            idle_threshold = 0;

            # Which monitor should the notifications be displayed on.
            monitor = 0;

            # The height of a single line. If the notification is one line it will be
            # filled out to be three lines.
            line_height = 3;

            # Draw a line of "separatpr_height" pixel height between two notifications
            separator_height = 2;

            # Padding between text and separator
            padding = 6;
            horizontal_padding = 6;

            # Define a color for the separator
            separator_color = "frame";

            # dmenu path
            dmenu = "${pkgs.rofi}/bin/rofi -dmenu -p dunst -theme glue_pro_blue";

            # Browser for opening urls in context menu.
            browser = "/run/current-system/sw/bin/firefox -new-tab";

            # Align icons left/right/off
            icon_position = "left";
            max_icon_size = 80;

            # Define frame size and color
            frame_width = 3;
            frame_color = "#8EC07C";
          };

          shortcuts = {
            close = "ctrl+space";
            close_all = "ctrl+shift+space";
          };

          urgency_low = {
            frame_color = "#3B7C87";
            foreground = "#3B7C87";
            background = "#191311";
            timeout = 4;
          };
          urgency_normal = {
            frame_color = "#5B8234";
            foreground = "#5B8234";
            background = "#191311";
            timeout = 6;
          };

          urgency_critical = {
            frame_color = "#B7472A";
            foreground = "#B7472A";
            background = "#191311";
            timeout = 8;
          };
        };

        # Set up mako, a notification deamon for wayland
        programs.mako.enable = isWayland;
        programs.mako.backgroundColor = "#191311";
        programs.mako.borderColor = "#3B7C87";
        programs.mako.borderSize = 3;
        programs.mako.defaultTimeout = 6000;
        programs.mako.font = "${config.my.fonts.monospace} ${toString (builtins.floor config.my.fonts.size)}";

        # Set up kanshi (which kinda is an autorandr for wayland)
        services.kanshi.enable = isWayland;
        services.kanshi.profiles = {
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

        # Sway user configs
        wayland.windowManager.sway.enable = config.my.sway.enable;
        wayland.windowManager.sway.extraSessionCommands = ''
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
        wayland.windowManager.sway.config = let
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
            "${modifier}+l" = "exec ${config.my.sway.lockCommand}";

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
            "${modifier}+Shift+e" = "exec ${config.my.sway.package}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' '${config.my.sway.package}/bin/swaymsg exit'";
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
          input."type:keyboard".xkb_file = toString (pkgs.writeText "us-dvorak-compose" ''
            // This file defines my own custom keymap. More information about which
            // parts that gets included are available in the different subfolders in:
            // ${pkgs.xorg.xkeyboardconfig}/share/X11/xkb/
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
          '');

          # Set wallpaper for all outputs
          output."*".bg = "${config.my.sway.wallpaperPackage}/default.jpg fill";

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

        services.swayidle.enable = config.my.sway.enable;
        services.swayidle.timeouts = [
          { timeout = 300; command = config.my.sway.lockCommand; }
          { timeout = 600; command = "${config.my.sway.package}/bin/swaymsg 'output * dpms off'"; }
        ];
        services.swayidle.events = [
          { event = "after-resume"; command = "${config.my.sway.package}/bin/swaymsg 'output * dpms on'"; }
          { event = "before-sleep"; command = config.my.sway.lockCommand; }
        ];

        programs.waybar.enable = config.my.sway.enable;
        programs.waybar.systemd.enable = config.my.sway.enable;
        programs.waybar.systemd.target = "sway-session.target";
        programs.waybar.style = pkgs.runCommandNoCC "waybar-styles.css" { } ''
          sed -e 's/font-family: /font-family: ${config.my.fonts.normal}, /'              \
              -e 's/font-size: 13px/font-size: ${toString (builtins.floor config.my.fonts.biggerSize)}px/' \
              ${pkgs.waybar}/etc/xdg/waybar/style.css > $out
        '';
        programs.waybar.settings = [{
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

        # Set up autorandr service to trigger on saved configurations
        programs.autorandr.enable = isX11;

        services.picom.enable = isX11;
        services.picom.vSync = (config.networking.hostName == "agrajag");

        services.flameshot.enable = isX11;
        services.pasystray.enable = isX11;
        services.network-manager-applet.enable = isX11;

        home.stateVersion = "20.09";
      };
  };
}
