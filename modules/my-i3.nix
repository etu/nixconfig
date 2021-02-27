{ config, lib, pkgs, ... }:
let
  cfg = config.my.i3;

  physlockCommand = "/run/wrappers/bin/physlock -ds";
  rofi = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };

  myI3Config = pkgs.writeText "i3wm.config" ''
    # i3 config file (v4)
    #
    # Please see https://i3wm.org/docs/userguide.html for a complete reference!

    ##
    ## Variables
    ##
      set $mod Mod4

      # Direction keys (Emacs logic)
      set $left b
      set $right f
      set $up p
      set $down n

      # Terminal emulator
      set $term ${pkgs.alacritty}/bin/alacritty

    ##
    ## Key bindings
    ##

    #
    # Basics:
    #
      # Start a terminal:
      bindsym $mod+Return exec $term

      # Kill focused window:
      bindsym $mod+Shift+quotedbl kill

      # Start your launcher:
      bindsym $mod+e exec ${rofi}/bin/rofi -show combi -theme glue_pro_blue

      # Use Mouse+$mod to drag floating windows to their wanted position:
      floating_modifier $mod

      # Reload the configuration file:
      bindsym $mod+Shift+c reload

      # Restart i3 inplace:
      bindsym $mod+Shift+l restart

      # Exit i3 (logs you out of your X session):
      bindsym $mod+Shift+greater exec "${cfg.package}/bin/i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' '${cfg.package}/bin/i3-msg exit'"

    #
    # Moving around:
    #
      # Move focus around:
      bindsym $mod+$left focus left
      bindsym $mod+$right focus right
      bindsym $mod+$up focus up
      bindsym $mod+$down focus down

      # Move focus around with cursor keys:
      bindsym $mod+Left focus left
      bindsym $mod+Down focus down
      bindsym $mod+Up focus up
      bindsym $mod+Right focus right

      # Move focused window:
      bindsym $mod+Shift+$left move left
      bindsym $mod+Shift+$right move right
      bindsym $mod+Shift+$up move up
      bindsym $mod+Shift+$down move down

      # Move focused window with cursor keys:
      bindsym $mod+Shift+Left move left
      bindsym $mod+Shift+Down move down
      bindsym $mod+Shift+Up move up
      bindsym $mod+Shift+Right move right

    #
    # Workspaces:
    #
      # Define the names for the workspaces we want to create:
      set $ws1 "1 - Misc"
      set $ws2 "2 - Web"
      set $ws3 "3 - Emacs"
      set $ws4 "4"
      set $ws5 "5"
      set $ws6 "6"
      set $ws7 "7"
      set $ws8 "8"
      set $ws9 "9"
      set $ws10 "10"

      # Switch to workspace:
      bindsym $mod+1 workspace number $ws1
      bindsym $mod+2 workspace number $ws2
      bindsym $mod+3 workspace number $ws3
      bindsym $mod+4 workspace number $ws4
      bindsym $mod+5 workspace number $ws5
      bindsym $mod+6 workspace number $ws6
      bindsym $mod+7 workspace number $ws7
      bindsym $mod+8 workspace number $ws8
      bindsym $mod+9 workspace number $ws9
      bindsym $mod+0 workspace number $ws10

      # Move focused container to workspace:
      bindsym $mod+Shift+1 move container to workspace number $ws1
      bindsym $mod+Shift+2 move container to workspace number $ws2
      bindsym $mod+Shift+3 move container to workspace number $ws3
      bindsym $mod+Shift+4 move container to workspace number $ws4
      bindsym $mod+Shift+5 move container to workspace number $ws5
      bindsym $mod+Shift+6 move container to workspace number $ws6
      bindsym $mod+Shift+7 move container to workspace number $ws7
      bindsym $mod+Shift+8 move container to workspace number $ws8
      bindsym $mod+Shift+9 move container to workspace number $ws9
      bindsym $mod+Shift+0 move container to workspace number $ws10

    #
    # Layouting:
    #
      # Split in horizontal orientation:
      bindsym $mod+h split h

      # Split in vertical orientation:
      bindsym $mod+v split v

      # Change layout of focused container:
      bindsym $mod+o layout stacking
      bindsym $mod+comma layout tabbed
      bindsym $mod+period layout toggle split

      # Fullscreen for the focused container:
      bindsym $mod+u fullscreen toggle

      # Toggle the current focus between tiling and floating mode:
      bindsym $mod+Shift+space floating toggle

      # Swap focus between the tiling area and the floating area:
      bindsym $mod+space focus mode_toggle

      # Focus the parent container
      bindsym $mod+a focus parent

      # Focus the child container
      bindsym $mod+d focus child

    #
    # Resizing:
    #
      # These bindings trigger as soon as you enter the resize mode
      mode "resize" {
        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym $left resize shrink width 10px
        bindsym $right resize grow width 10px
        bindsym $up resize shrink height 10px
        bindsym $down resize grow height 10px

        # You can also use the arrow keys:
        bindsym Left resize shrink width 10 px or 10 ppt
        bindsym Down resize grow height 10 px or 10 ppt
        bindsym Up resize shrink height 10 px or 10 ppt
        bindsym Right resize grow width 10 px or 10 ppt

        # Exit resize mode:
        bindsym Return mode "default"
        bindsym Escape mode "default"
        bindsym $mod+r mode "default"
      }

      # Enter resize mode:
      bindsym $mod+r mode "resize"

    #
    # Other keybindings:
    #
      # Printscreen:
      bindsym Print exec ${pkgs.flameshot}/bin/flameshot gui

      # Backlight:
      bindsym XF86MonBrightnessUp exec ${pkgs.acpilight}/bin/xbacklight -inc 10
      bindsym XF86MonBrightnessDown exec ${pkgs.acpilight}/bin/xbacklight -dec 10

      # Audio:
      bindsym XF86AudioMute exec pactl set-sink-mute @DEFAULT_SINK@ toggle
      bindsym XF86AudioLowerVolume exec pactl set-sink-volume @DEFAULT_SINK@ -10%
      bindsym XF86AudioRaiseVolume exec pactl set-sink-volume @DEFAULT_SINK@ +10%
      bindsym XF86AudioMicMute exec pactl set-source-mute @DEFAULT_SOURCE@ toggle

      # Misc buttons:
      bindsym XF86Tools exec emacs
      bindsym XF86Display exec ${pkgs.autorandr}/bin/autorandr -cf
      bindsym XF86Favorites exec emacs

      # Rofi emoji picker:
      bindsym $mod+i exec ${rofi}/bin/rofi -show emoji -theme glue_pro_blue

      # Lock the screen:
      bindsym $mod+l exec "${physlockCommand}"

    ##
    ## Visuals
    ##
      # Font for window titles:
      font pango:monospace 8

      # i3bar settings:
      bar {
        status_command i3status
        tray_output primary
        position top
        font pango:monospace 10
      }
  '';

in
{
  options.my.i3.enable = lib.mkEnableOption "Enables i3wm and auto login for my user";
  options.my.i3.package = lib.mkOption {
    type = lib.types.package;
    default = pkgs.i3;
    defaultText = "pkgs.i3";
    description = "Which i3 package to use";
  };

  config = lib.mkIf cfg.enable {
    # Libinput
    services.xserver.libinput.enable = true;

    # Loginmanager
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.autoLogin.enable = true;
    services.xserver.displayManager.autoLogin.user = config.my.user.username;

    # Needed for autologin
    services.xserver.displayManager.defaultSession = "none+i3";

    # Set up i3
    services.xserver.windowManager.i3.enable = true;
    services.xserver.windowManager.i3.package = cfg.package;
    services.xserver.windowManager.i3.configFile = myI3Config;
    services.xserver.windowManager.i3.extraSessionCommands = ''
      # Keybind:                           ScrollLock -> Compose,      <> -> Compose
      ${pkgs.xorg.xmodmap}/bin/xmodmap -e 'keycode 78 = Multi_key' -e 'keycode 94 = Multi_key'

      if test -L ~/.background; then
        ${pkgs.feh}/bin/feh --bg-fill ~/.background
      fi
    '';

    # Set up services needed for gnome stuff for evolution
    services.gnome3.evolution-data-server.enable = true;
    services.gnome3.gnome-keyring.enable = true;

    # Enable autorandr for screen setups.
    services.autorandr.enable = true;

    # Install aditional packages
    environment.systemPackages = with pkgs; [
      alacritty
      evince
      evolution
      gnome3.adwaita-icon-theme # Icons for gnome packages that sometimes use them but don't depend on them
      scrot
      pavucontrol
    ];

    # Configure TERMINAL for i3-sensible-terminal
    environment.variables.TERMINAL = "alacritty";

    # Enable physlock and make a suid wrapper for it
    services.physlock.enable = true;
    services.physlock.allowAnyUser = true;

    # Enable auto locking of the screen
    services.xserver.xautolock.enable = true;
    services.xserver.xautolock.locker = physlockCommand;
    services.xserver.xautolock.enableNotifier = true;
    services.xserver.xautolock.notify = 10;
    services.xserver.xautolock.notifier = "${pkgs.libnotify}/bin/notify-send \"Locking in 10 seconds\"";
    services.xserver.xautolock.time = 3;
  };
}
