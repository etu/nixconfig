{ config, lib, pkgs, ... }:

let
  cfg = config.my.sway;

  rofi = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };

  # TODO:
  # - Network manager applet
  # - Pulse audio systray
  # - Dunst
  # - Media keys

  # Here I define a custom XKB keymap to pass to sway for all my keyboard
  # related settings in one go. No xmodmap or thing like that needed.
  myKeymap = pkgs.writeText "us-dvorak-compose" ''
    // This file defines my own custom keymap. More information about which
    // parts that gets included are available in the different subfolders in:
    // ${pkgs.xorg.xkeyboardconfig}/share/X11/xkb/
    xkb_keymap {
    	xkb_keycodes  { include "evdev+aliases(qwerty)"	};
    	xkb_types     { include "default"	};
    	xkb_compat    { include "complete"	};
    	xkb_symbols   {
    		include "pc+us(dvorak)+inet(evdev)+ctrl(nocaps)+eurosign(e)+kpdl(dot)"

    		// Less than/Greater than/Pipe key on Swedish keyboards becomes Compose
    		replace key <LSGT> { [ Multi_key ] };

    		// Scroll Lock becomes Compose
    		replace key <SCLK> { [ Multi_key ] };
    	};
    	xkb_geometry  { include "pc(pc105)"	};
    };
  '';

  mySwayConfig = pkgs.writeText "sway.config" ''
    # sway config file
    #
    # Read `man 5 sway` for a complete reference.

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
      set $term ${pkgs.kitty}/bin/kitty

    ##
    ## Output configuration
    ##
      # Default wallpaper
      output * bg ~/.background fill

      # Example configuration:
      #
      # Read manpage at `man 5 sway-output`.
      #
      #   output HDMI-A-1 resolution 1920x1080 position 1920,0
      #
      # You can get the names of your outputs by running: swaymsg -t get_outputs

    ##
    ## Idle configuration
    ##
      # This will lock your screen after 300 seconds of inactivity, then turn off
      # your displays after another 300 seconds, and turn your screens back on when
      # resumed. It will also lock your screen before your computer goes to sleep.
      exec ${pkgs.swayidle}/bin/swayidle -w \
        timeout 300 '${pkgs.swaylock-effects}/bin/swaylock -f --effect-greyscale --effect-pixelate 5 -S' \
        timeout 600 '${cfg.package}/bin/swaymsg "output * dpms off"' \
             resume '${cfg.package}/bin/swaymsg "output * dpms on"' \
        before-sleep '${pkgs.swaylock-effects}/bin/swaylock -f --effect-greyscale --effect-pixelate 5 -S'

    ##
    ## Input configuration
    ##
      # You can get the names of your inputs by running: swaymsg -t get_inputs
      # Read `man 5 sway-input` for more information about this section.

      input "type:keyboard" {
        xkb_file ${myKeymap}
      }

      # Example configuration:
      #
      #   input "2:14:SynPS/2_Synaptics_TouchPad" {
      #       dwt enabled
      #       tap enabled
      #       natural_scroll enabled
      #       middle_emulation enabled
      #   }

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
      bindsym $mod+e exec ${rofi}/bin/rofi -show combi -theme glue_pro_blue | xargs swaymsg exec --

      # Use Mouse+$mod to drag floating windows to their wanted position:
      floating_modifier $mod

      # Reload the configuration file:
      bindsym $mod+Shift+c reload

      # Exit sway (logs you out of your Wayland session)
      bindsym $mod+Shift+e exec ${cfg.package}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' '${cfg.package}/bin/swaymsg exit'

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
        bindsym Left resize shrink width 10 px
        bindsym Down resize grow height 10 px
        bindsym Up resize shrink height 10 px
        bindsym Right resize grow width 10 px

        # Exit resize mode:
        bindsym Return mode "default"
        bindsym Escape mode "default"
        bindsym $mod+r mode "default"
      }

      # Enter resize mode:
      bindsym $mod+r mode "resize"

    #
    # Scratchpad:
    #
      # Move window to scratchpad:
      bindsym $mod+Shift+minus move scratchpad

      # Show scratchpad window and cycle through them:
      bindsym $mod+minus scratchpad show

    #
    # Other keybindings:
    #
      # Printscreen:
      bindsym Print exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy area
      bindsym Shift+Print exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save area

    ##
    ## Visuals
    ##

    #
    # Start things:
    #
      # Notification deamon:
      exec mako

    #
    # Status Bar:
    #
    # Read `man 5 sway-bar` for more information about this section.
      bar {
        position top

        status_command ${pkgs.i3status}/bin/i3status
      }
  '';

in {
  options.my.sway.enable = lib.mkEnableOption "Enables sway and auto login for my user";
  options.my.sway.package = lib.mkOption {
    type = lib.types.package;
    default = pkgs.sway;
    defaultText = "pkgs.sway";
    description = "Which sway package to use";
  };

  config = lib.mkIf cfg.enable {
    programs.sway.enable = true;

    # Loginmanager
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.autoLogin.enable = true;
    services.xserver.displayManager.autoLogin.user = config.my.user.username;

    # Set up services needed for gnome stuff for evolution
    services.gnome3.evolution-data-server.enable = true;
    services.gnome3.gnome-keyring.enable = true;

    # Install aditional packages
    environment.systemPackages = with pkgs; [
      evince
      gnome3.adwaita-icon-theme # Icons for gnome packages that sometimes use them but don't depend on them
      gnome3.evolution
      pavucontrol
    ];

    # Needed for autologin
    services.xserver.displayManager.defaultSession = "sway";

    environment.etc."sway/config".source = mySwayConfig;
  };
}
