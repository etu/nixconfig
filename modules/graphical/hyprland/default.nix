{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.hyprland.enable = lib.mkEnableOption "Enable graphical hyprland settings";
  options.etu.graphical.hyprland.enableXwayland =
    lib.mkEnableOption "Enable xwayland"
    // {
      default = true;
    };

  config = lib.mkIf config.etu.graphical.hyprland.enable {
    # Enable a wayland build of Emacs.
    etu.base.emacs.package = "wayland";

    # Set environment variable to make Electron and Chromium
    # applications run under wayland.
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    # Install some programs
    etu.user.extraUserPackages = [
      pkgs.bluetuith
      pkgs.evince
      pkgs.pavucontrol
      pkgs.wdisplays
      pkgs.wlr-randr
      pkgs.wofi
      pkgs.wofi-emoji
    ];

    # Enable hyprland
    programs.hyprland.enable = true;

    # Make sure to start the home-manager activation before I log in.
    systemd.services."home-manager-${config.etu.user.username}" = {
      before = ["display-manager.service"];
      wantedBy = ["multi-user.target"];
    };

    # Enable the X11 windowing system (for the loginmanager).
    services.xserver.enable = true;

    # Loginmanager
    services.xserver.displayManager.lightdm.enable = true;

    # Needed for autologin
    services.displayManager.autoLogin.enable = true;
    services.displayManager.autoLogin.user = config.etu.user.username;
    services.displayManager.defaultSession = "hyprland";

    # Don't have xterm as a session.
    services.xserver.desktopManager.xterm.enable = false;

    # Keyboard layout used by X11 (and the login screen).
    services.xserver.xkb.layout = "us";
    services.xserver.xkb.options = "eurosign:e,ctrl:nocaps,numpad:mac,kpdl:dot";
    services.xserver.xkb.variant = "dvorak";

    # Set up Pipewire
    services.pipewire.enable = true;
    services.pipewire.alsa.enable = true;
    services.pipewire.pulse.enable = true;
    services.pipewire.jack.enable = true;

    # Enable PAM settings for hyprlock to work.
    security.pam.services.hyprlock = {};

    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Enable hypridle for idle management
      services.hypridle.enable = true;
      services.hypridle.settings = {
        general = {
          after_sleep_cmd = "hyprctl dispatch dpms on";
          before_sleep_cmd = "loginctl lock-session";
          ignore_dbus_inhibit = false;
          lock_cmd = "hyprlock";
        };

        listener = [
          {
            timeout = 300;
            on-timeout = "loginctl lock-session";
          }
          {
            timeout = 600;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on; loginctl lock-session";
          }
        ];
      };

      # Enable hyprlock for screen locking
      programs.hyprlock.enable = true;
      programs.hyprlock.settings = {
        general = {
          disable_loading_bar = true;
          grace = 0;
          hide_cursor = true;
          no_fade_in = false;
        };
        background = [
          {
            path = "screenshot";
            blur_passes = 3;
            blur_size = 8;
          }
        ];
        input-field = [
          {
            size = "200, 50";
            position = "0, -80";
            monitor = "";
            dots_center = true;
            fade_on_empty = false;
            font_color = "rgb(202, 211, 245)";
            inner_color = "rgb(91, 96, 120)";
            outer_color = "rgb(24, 25, 38)";
            outline_thickness = 5;
            placeholder_text = ''<span foreground="##cad3f5">Password...</span>'';
            shadow_passes = 2;
          }
        ];
        label = [
          {
            monitor = "";
            position = "0, 240";
            halign = "center";
            valign = "center";
            text = "$TIME";
            text_align = "center";
            color = "rgba(200, 200, 200, 1.0)";
            font_size = 96;
            font_family = "Noto Sans";
            rotate = 0;
          }
        ];
      };

      wayland.windowManager.hyprland.enable = true;
      wayland.windowManager.hyprland.plugins = [
        pkgs.hyprlandPlugins.hy3
      ];

      # Option to enable or disable xwayland.
      wayland.windowManager.hyprland.xwayland.enable = config.etu.graphical.hyprland.enableXwayland;

      # Define submaps
      wayland.windowManager.hyprland.extraConfig = ''
        # Define a key to enter the submap to resize
        bind = $mod, R, submap, resize

        # Define the submap to resize
        submap = resize

        # With the following keybinds
        binde = , left, resizeactive, 10 0
        binde = , right, resizeactive, -10 0
        binde = , down, resizeactive, 0 -10
        binde = , up, resizeactive, 0 10

        binde = , B, resizeactive, 10 0
        binde = , F, resizeactive, -10 0
        binde = , N, resizeactive, 0 -10
        binde = , P, resizeactive, 0 10

        # And to reset
        bind = , Escape, submap, reset
        bind = , Return, submap, reset
        bind = $mod, R, submap, reset

        # And then exit the submap
        submap = reset
      '';

      wayland.windowManager.hyprland.settings = let
        # Shortcut for pactl
        pactl = "${config.hardware.pulseaudio.package}/bin/pactl";

        # Direction keys (Emacs logic)
        left = "B";
        right = "F";
        up = "P";
        down = "N";
      in {
        "$mod" = "SUPER";

        # Keymap
        input.kb_file = toString config.etu.graphical.xkb-keymap;

        # Make the laptop not scale the internal display.
        monitor = [
          "eDP-1, preferred, auto, 1"
        ];

        # Use the hy3 plugin as layout.
        general.layout = "hy3";

        # Restart hypridle on every reload of hyprland to make sure it
        # has it's latest settings.
        exec = [
          "systemctl --user restart hypridle.service"
          "systemctl --user restart kanshi.service"
        ];
        exec-once = [
          "${pkgs.mako}/bin/mako"
        ];

        # Specify window rules.
        windowrulev2 = [
          # Set opacity for certain programs
          "opacity 0.9,class:(Alacritty)"
          "opacity 0.9,class:(emacs)"
        ];

        # Configure general settings for hy3
        plugin.hy3.autotile.enable = true;

        # Repeatable keybinds
        binde = [
          # Backlight:
          ", XF86MonBrightnessUp, exec, ${pkgs.acpilight}/bin/xbacklight -inc 10"
          ", XF86MonBrightnessDown, exec, ${pkgs.acpilight}/bin/xbacklight -dec 10"
        ];

        # Keybinds that works on lock screen
        bindl = [
          # Audio:
          ", XF86AudioMute, exec, ${pactl} set-sink-mute @DEFAULT_SINK@ toggle"
          ", XF86AudioMicMute, exec, ${pactl} set-source-mute @DEFAULT_SOURCE@ toggle"
        ];

        # Repeatable keybinds that works on the lock screen
        bindel = [
          # Audio:
          ", XF86AudioLowerVolume, exec, ${pactl} set-sink-volume @DEFAULT_SINK@ -10%"
          ", XF86AudioRaiseVolume, exec, ${pactl} set-sink-volume @DEFAULT_SINK@ +10%"
        ];

        # Binds for mouse things
        bindm = [
          # Move/resize windows with mod + LMB/RMB and dragging
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];

        # Keybinds
        bind = [
          # My keybinds
          "$mod, Return, exec, ${config.etu.graphical.terminal.terminalPath}" # Run terminal
          "$mod, E, exec, wofi --show run" # Run launcher
          "$mod, I, exec, wofi-emoji" # Run emoji picker
          "$mod SHIFT, apostrophe, hy3:killactive" # Kill currently focused window
          "$mod SHIFT, space, togglefloating" # Toggle floating for window
          "$mod, U, fullscreen" # Toggle fullscreen for window

          # Printscreen:
          ", Print, exec, ${pkgs.grim}/bin/grim -g \"$(${pkgs.slurp}/bin/slurp)\" - | ${pkgs.swappy}/bin/swappy -f -"

          # Launch screen locker
          "$mod, L, exec, loginctl lock-session"

          # Move focus around (emacs directions):
          "$mod, ${left}, hy3:movefocus, l"
          "$mod, ${right}, hy3:movefocus, r"
          "$mod, ${up}, hy3:movefocus, u"
          "$mod, ${down}, hy3:movefocus, d"

          # Move focus around with cursor keys:
          "$mod, left, hy3:movefocus, l"
          "$mod, right, hy3:movefocus, r"
          "$mod, up, hy3:movefocus, u"
          "$mod, down, hy3:movefocus, d"

          # Move window around (emacs directions):
          "$mod SHIFT, ${left}, hy3:movewindow, l"
          "$mod SHIFT, ${right}, hy3:movewindow, r"
          "$mod SHIFT, ${up}, hy3:movewindow, u"
          "$mod SHIFT, ${down}, hy3:movewindow, d"

          # Move window around with cursor keys:
          "$mod SHIFT, left, hy3:movewindow, l"
          "$mod SHIFT, right, hy3:movewindow, r"
          "$mod SHIFT, up, hy3:movewindow, u"
          "$mod SHIFT, down, hy3:movewindow, d"

          # Hy3 specific keybinds
          "$mod, V, hy3:makegroup, v" # Make a vertical split
          "$mod, H, hy3:makegroup, h" # Make a horizontal split
          "$mod, comma, hy3:makegroup, tab" # Make a tabbed layout
          "$mod, period, hy3:changegroup, toggletab" # Toggle between tabbad and untabbed
          "$mod, O, hy3:changegroup, opposite" # Change between horizontal and vertical layout
          "$mod, A, hy3:changefocus, raise" # Change focus to parent node
          "$mod, D, hy3:changefocus, lower" # Change focus to child node
          "$mod, T, hy3:expand, expand" # Make the current node expand over other nodes
          "$mod SHIFT, T, hy3:expand, base" # Undo all expansions

          # Switch workspaces with mainMod + [0-9]
          "$mod, 1, workspace, 1"
          "$mod, 2, workspace, 2"
          "$mod, 3, workspace, 3"
          "$mod, 4, workspace, 4"
          "$mod, 5, workspace, 5"
          "$mod, 6, workspace, 6"
          "$mod, 7, workspace, 7"
          "$mod, 8, workspace, 8"
          "$mod, 9, workspace, 9"
          "$mod, 0, workspace, 10"

          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          "$mod SHIFT, 1, hy3:movetoworkspace, 1"
          "$mod SHIFT, 2, hy3:movetoworkspace, 2"
          "$mod SHIFT, 3, hy3:movetoworkspace, 3"
          "$mod SHIFT, 4, hy3:movetoworkspace, 4"
          "$mod SHIFT, 5, hy3:movetoworkspace, 5"
          "$mod SHIFT, 6, hy3:movetoworkspace, 6"
          "$mod SHIFT, 7, hy3:movetoworkspace, 7"
          "$mod SHIFT, 8, hy3:movetoworkspace, 8"
          "$mod SHIFT, 9, hy3:movetoworkspace, 9"
          "$mod SHIFT, 0, hy3:movetoworkspace, 10"
        ];
      };

      home.file = {
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
      }; # END home.file
    };
  };
}
