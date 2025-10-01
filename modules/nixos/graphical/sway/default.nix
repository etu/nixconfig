{
  config,
  perSystem,
  lib,
  pkgs,
  ...
}:
{
  options.etu.graphical.sway = {
    enable = lib.mkEnableOption "Enables sway and auto login for my user";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.sway;
      description = "Which sway package to use";
      readOnly = true;
    };
    wallpaper = lib.mkOption {
      type = lib.types.str;
      default = builtins.toString perSystem.self.spaceWallpapers;
      description = "Wallpaper to use for sway";
    };
    lockWallpaper = lib.mkOption {
      type = lib.types.str;
      default = "screenshot";
      description = "Wallpaper to use for lockscreen";
    };
    enableSuspendOnTimeout = lib.mkEnableOption "Lock the screen before suspending" // {
      default = true;
    };
  };

  config = lib.mkIf config.etu.graphical.sway.enable {
    # Enable a wayland build of Emacs.
    etu.base.emacs.package = "wayland";

    # Install packages using home manager.
    etu.user.extraUserPackages = [
      pkgs.bluetuith
      pkgs.evince
      pkgs.adwaita-icon-theme # Icons for gnome packages that sometimes use them but don't depend on them
      pkgs.pavucontrol
      pkgs.wdisplays
      pkgs.wlr-randr

      # Script to reload environment variables (if used nested sway
      # session and want chrome screen sharing to read the inner sway)
      (pkgs.writeShellScriptBin "sway-reload-env" ''
        ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd WAYLAND_DISPLAY SWAYSOCK
        ${pkgs.systemd}/bin/systemctl --user restart xdg-desktop-portal.service
        ${pkgs.systemd}/bin/systemctl --user restart xdg-desktop-portal-wlr.service
      '')
    ];

    programs.sway.enable = true;

    # Make sure to start the home-manager activation before I log in.
    systemd.services."home-manager-${config.etu.user.username}" = {
      before = [ "display-manager.service" ];
      wantedBy = [ "multi-user.target" ];
    };

    # Enable greetd as a non-graphical login manager.
    services.greetd.enable = true;
    services.greetd.settings = {
      default_session.command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd sway";
      default_session.user = "greeter";
      initial_session.command = "sway";
      initial_session.user = config.etu.user.username;
    };

    # Set up Pipewire
    services.pipewire.enable = true;
    services.pipewire.alsa.enable = true;
    services.pipewire.pulse.enable = true;
    services.pipewire.jack.enable = true;

    # Set up upower to be able to get battery levels of connected devices.
    services.upower.enable = true;

    # Set up XDG Portals
    xdg.portal.enable = true;
    xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Enable rofi home manager module.
      programs.rofi.enable = true;
      programs.rofi.font = "${config.etu.graphical.theme.fonts.monospace} ${toString config.etu.graphical.theme.fonts.size}";

      # Set up a wallpaper manager.
      services.wpaperd.enable = true;
      services.wpaperd.settings = {
        default = {
          duration = "30m";
          mode = "center";
        };
        any.path = config.etu.graphical.sway.wallpaper;
      };

      # Enable and import network-manager-applet
      services.network-manager-applet.enable = true;

      # Enable the blueman applet service.
      services.blueman-applet.enable = true;

      # Enable the playerctld to be able to control music players and mpris-proxy to proxy bluetooth devices.
      services.playerctld.enable = true;
      services.mpris-proxy.enable = true;

      # Set up easyeffects
      services.easyeffects.enable = true;
      services.easyeffects.extraPresets.my-preset = {
        input = {
          blocklist = [ ];
          plugins_order = [ "rnnoise#0" ];
          "rnnoise#0" = {
            bypass = false;
            enable-vad = true;
            input-gain = 0.0;
            model-name = "";
            output-gain = 0.0;
            release = 20.0;
            vad-thres = 95.0;
            wet = 0.0;
          };
        };
      };
      services.easyeffects.preset = "my-preset";

      # Configure swayidle for automatic screen locking
      services.swayidle.enable = true;
      services.swayidle.events = [
        {
          event = "before-sleep";
          command = "${pkgs.swaylock-effects}/bin/swaylock";
        }
        {
          event = "lock";
          command = "${pkgs.swaylock-effects}/bin/swaylock";
        }
      ];
      services.swayidle.timeouts = [
        {
          timeout = 300;
          command = "${pkgs.swaylock-effects}/bin/swaylock";
        }
      ]
      ++ lib.optionals config.etu.graphical.sway.enableSuspendOnTimeout [
        {
          timeout = 600;
          command = "${pkgs.systemd}/bin/systemctl suspend";
        }
      ];

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

        # Set the TERMINAL environment variable for rofi-sensible-terminal
        TERMINAL = config.etu.graphical.terminal.terminalName;
      };

      # Configure swaylock
      programs.swaylock.enable = true;
      programs.swaylock.package = pkgs.swaylock-effects;
      programs.swaylock.settings = {
        daemonize = true;
        clock = true;
        timestr = "%k:%M";
        datestr = "%Y-%m-%d";
        show-failed-attempts = true;
      }
      // (lib.optionalAttrs (config.etu.graphical.sway.lockWallpaper == "screenshot") {
        indicator = true;
        screenshots = true;
        effect-blur = "5x5";
      })
      // (lib.optionalAttrs (config.etu.graphical.sway.lockWallpaper != "screenshot") {
        image = config.etu.graphical.sway.lockWallpaper;
      });

      wayland.systemd.target = "sway-session.target";

      # Sway user configs
      wayland.windowManager.sway = {
        enable = true;
        systemd.enable = true;

        config =
          let
            rofi = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
            pactl = "${config.services.pulseaudio.package}/bin/pactl";

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
              "${modifier}+Return" = "exec ${config.etu.graphical.terminal.terminalPath}";

              # Run Launcher
              "${modifier}+e" =
                "exec ${rofi}/bin/rofi -show combi -modi combi -combi-modes 'window,drun' | xargs swaymsg exec --";

              # Run rofi emoji picker
              "${modifier}+i" = "exec ${rofi}/bin/rofi -show emoji";

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
              XF86AudioPrev = "exec ${pkgs.playerctl}/bin/playerctl previous";
              XF86AudioPlay = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
              XF86AudioNext = "exec ${pkgs.playerctl}/bin/playerctl next";

              # Misc buttons:
              XF86Tools = "exec ${config.services.emacs.package}/bin/emacs";
              XF86Favorites = "exec ${config.services.emacs.package}/bin/emacs";

              # Launch screen locker
              "${modifier}+l" = "exec swaylock";

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
                "exec ${config.etu.graphical.sway.package}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' '${config.etu.graphical.sway.package}/bin/swaymsg exit'";
            }
            // lib.optionalAttrs config.etu.games.mumble.enable {
              # Add PTT button for mumble in the gaming module:
              "--no-repeat Alt_R" =
                "exec ${pkgs.glib}/bin/gdbus call -e -d net.sourceforge.mumble.mumble -o / -m net.sourceforge.mumble.Mumble.startTalking";
              "--no-repeat --release Alt_R" =
                "exec ${pkgs.glib}/bin/gdbus call -e -d net.sourceforge.mumble.mumble -o / -m net.sourceforge.mumble.Mumble.stopTalking";
            };

            modes.resize = {
              "${left}" = "resize shrink width 10px"; # Pressing left will shrink the window’s width.
              "${right}" = "resize grow width 10px"; # Pressing right will grow the window’s width.
              "${up}" = "resize shrink height 10px"; # Pressing up will shrink the window’s height.
              "${down}" = "resize grow height 10px"; # Pressing down will grow the window’s height.

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
              names = [ config.etu.graphical.theme.fonts.monospace ];
              size = config.etu.graphical.theme.fonts.size + 0.0;
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
              {
                app_id = ".blueman-manager-wrapped";
                title = "Bluetooth Devices";
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
            input."type:keyboard".xkb_model = config.etu.graphical.xkb-keymap.model;
            input."type:keyboard".xkb_layout = config.etu.graphical.xkb-keymap.layout;
            input."type:keyboard".xkb_options = config.etu.graphical.xkb-keymap.options;
            input."type:keyboard".xkb_variant = config.etu.graphical.xkb-keymap.variant;

            # Enable titlebars
            window.titlebar = true;
            floating.titlebar = true;

            startup = [
              { command = "${pkgs.mako}/bin/mako"; }

              # Import variables needed for screen sharing and gnome3 pinentry to work.
              { command = "${pkgs.dbus}/bin/dbus-update-activation-environment WAYLAND_DISPLAY"; }

              # Import user environment PATH to systemctl as user and then restart the xdg-desktop-portal
              # This is to get xdg-open to work in flatpaks to be able to open links inside of flatpaks.
              {
                command = "${config.systemd.package}/bin/systemctl --user import-environment PATH && ${config.systemd.package}/bin/systemctl --user restart xdg-desktop-portal.service";
              }
            ];

            # Disable the default bar
            bars = [ { mode = "invisible"; } ];
          };
      }; # END sway
    }; # END home-manager
  };
}
