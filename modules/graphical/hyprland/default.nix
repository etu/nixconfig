{
  config,
  hy3,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.hyprland.enable = lib.mkEnableOption "Enable graphical hyprland settings";

  config = lib.mkIf config.etu.graphical.hyprland.enable {
    # Enable hyprland
    programs.hyprland.enable = true;

    # Install some programs
    environment.systemPackages = [
      pkgs.wdisplays
      pkgs.wlr-randr
      pkgs.wofi
    ];

    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.file = {
        # My hyprland config
        ".config/hypr/hyprland.conf".text = let
          rofi = pkgs.rofi.override {plugins = [pkgs.rofi-emoji];};

          # Set default modifier
          modifier = "SUPER";

          # Direction keys (Emacs logic)
          left = "B";
          right = "F";
          up = "P";
          down = "N";
        in ''
          # See https://wiki.hyprland.org/Configuring/Monitors/
          monitor=,preferred,auto,auto

          # See https://wiki.hyprland.org/Configuring/Keywords/ for more

          # Execute your favorite apps at launch
          # exec-once = waybar & hyprpaper & firefox

          # Source a file (multi-file configs)
          # source = ~/.config/hypr/myColors.conf

          # Some default env vars.
          env = XCURSOR_SIZE,24

          # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
          input {
            kb_file = ${toString config.etu.graphical.xkb-keymap}

            follow_mouse = 1

            touchpad {
              natural_scroll = no
            }

            sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
          }

          general {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            gaps_in = 5
            gaps_out = 20
            border_size = 2
            col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
            col.inactive_border = rgba(595959aa)

            layout = dwindle
          }

          decoration {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            rounding = 10

            blur {
              enabled = true
              size = 3
              passes = 1
            }

            drop_shadow = yes
            shadow_range = 4
            shadow_render_power = 3
            col.shadow = rgba(1a1a1aee)
          }

          animations {
            enabled = yes

            # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more
            bezier = myBezier, 0.05, 0.9, 0.1, 1.05

            animation = windows, 1, 7, myBezier
            animation = windowsOut, 1, 7, default, popin 80%
            animation = border, 1, 10, default
            animation = borderangle, 1, 8, default
            animation = fade, 1, 7, default
            animation = workspaces, 1, 6, default
          }

          dwindle {
            # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
            pseudotile = yes # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
            preserve_split = yes # you probably want this
          }

          master {
            # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
            new_is_master = true
          }

          gestures {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            workspace_swipe = off
          }

          # Example per-device config
          # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
          device:epic-mouse-v1 {
            sensitivity = -0.5
          }

          # Make the laptop not scale the internal display.
          monitor = eDP-1, 1920x1080@60, 0x0, 1

          # Example windowrule v1
          # windowrule = float, ^(kitty)$
          # Example windowrule v2
          # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
          # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more

          # My keybinds
          bind = ${modifier}, Return, exec, ${config.etu.graphical.terminal.terminalPath}
          bind = ${modifier} SHIFT, apostrophe, killactive,
          bind = ${modifier} SHIFT, space, togglefloating,
          bind = ${modifier}, E, exec, ${rofi}/bin/rofi -show combi -theme glue_pro_blue | xargs exec --
          bind = ${modifier}, I, exec, ${rofi}/bin/rofi -show emoji -theme glue_pro_blue
          bind = ${modifier}, R, exec, ${pkgs.wofi}/bin/wofi --show drun

          # Move focus around (emacs directions):
          bind = ${modifier}, ${left}, movefocus, l
          bind = ${modifier}, ${right}, movefocus, r
          bind = ${modifier}, ${up}, movefocus, u
          bind = ${modifier}, ${down}, movefocus, d

          # Move focus around with cursor keys:
          bind = ${modifier}, left, movefocus, l
          bind = ${modifier}, right, movefocus, r
          bind = ${modifier}, up, movefocus, u
          bind = ${modifier}, down, movefocus, d

          # Switch workspaces with mainMod + [0-9]
          bind = ${modifier}, 1, workspace, 1
          bind = ${modifier}, 2, workspace, 2
          bind = ${modifier}, 3, workspace, 3
          bind = ${modifier}, 4, workspace, 4
          bind = ${modifier}, 5, workspace, 5
          bind = ${modifier}, 6, workspace, 6
          bind = ${modifier}, 7, workspace, 7
          bind = ${modifier}, 8, workspace, 8
          bind = ${modifier}, 9, workspace, 9
          bind = ${modifier}, 0, workspace, 10

          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          bind = ${modifier} SHIFT, 1, movetoworkspace, 1
          bind = ${modifier} SHIFT, 2, movetoworkspace, 2
          bind = ${modifier} SHIFT, 3, movetoworkspace, 3
          bind = ${modifier} SHIFT, 4, movetoworkspace, 4
          bind = ${modifier} SHIFT, 5, movetoworkspace, 5
          bind = ${modifier} SHIFT, 6, movetoworkspace, 6
          bind = ${modifier} SHIFT, 7, movetoworkspace, 7
          bind = ${modifier} SHIFT, 8, movetoworkspace, 8
          bind = ${modifier} SHIFT, 9, movetoworkspace, 9
          bind = ${modifier} SHIFT, 0, movetoworkspace, 10

          # See https://wiki.hyprland.org/Configuring/Keywords/ for more
          # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
          bind = ${modifier}, M, exit,
          bind = ${modifier}, A, pseudo, # dwindle
          bind = ${modifier}, J, togglesplit, # dwindle

          # Scroll through existing workspaces with mainMod + scroll
          bind = ${modifier}, mouse_down, workspace, e+1
          bind = ${modifier}, mouse_up, workspace, e-1

          # Move/resize windows with mainMod + LMB/RMB and dragging
          bindm = ${modifier}, mouse:272, movewindow
          bindm = ${modifier}, mouse:273, resizewindow

          # Import the hyprland hy3 plugin to make it more like sway
          plugin = ${hy3}/lib/libhy3.so
        '';
      };
    };
  };
}
