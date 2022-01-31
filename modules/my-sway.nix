{ config, lib, pkgs, ... }:
let
  cfg = config.my.sway;

  rofi = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
  pactl = "${config.hardware.pulseaudio.package}/bin/pactl";

  # Lock command
  lockCommand = "${pkgs.swaylock-effects}/bin/swaylock -f --effect-greyscale --effect-pixelate 5 -S";

  wallpaper = pkgs.stdenv.mkDerivation {
    pname = "wallpaper";
    version = "2021-03-19";

    src = pkgs.fetchFromGitHub {
      owner = "hexive";
      repo = "sunpaper";
      rev = "bc6c684825b74a14951d8f4ba940227296861d3c";
      sha256 = "0855x445fqi4yzif6n9g24j9jf6ah9kq7lkq8pxv4cmr34mvd5rg";
    };

    installPhase = let
      # upper left corner of rectangle.
      base = { x = 3835; y = 35; };
      rect = {
        xy0 = "${toString base.x},${toString base.y}";
        # Add 720p to the coordinates to find the lower right corner.
        xy1 = "${toString (base.x + 1280)},${toString (base.y + 720)}";
      };
      # Add some pixels to the base coordinates to place the text nicely.
      text.xy = "${toString (base.x + 1115)},${toString (base.y + 60)}";
    in ''
      mkdir -p $out

      # Resize for normal background
      ${pkgs.graphicsmagick}/bin/gm convert -crop 7680x2160+0+375 -resize 5120x1440 images/Lakeside/5.jpg $out/default.jpg

      # Draw a 720p rectangle on top
      ${pkgs.graphicsmagick}/bin/gm convert -fill '#FFFFFFBB' -draw 'rectangle ${rect.xy0} ${rect.xy1}' $out/default.jpg 720pfigure.jpg

      # Draw a text on top of this
      ${pkgs.imagemagickBig}/bin/convert -fill '#FFFFFF' -pointsize 72 -draw 'text ${text.xy} "720p"' 720pfigure.jpg $out/720pfigure.jpg
    '';
  };

  # TODO:
  # - Network manager applet
  # - Media keys (Missing: XF86Display)

  # Here I define a custom XKB keymap to pass to sway for all my keyboard
  # related settings in one go. No xmodmap or thing like that needed.
  myKeymap = pkgs.writeText "us-dvorak-compose" ''
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
  '';

  mySwayConfig = pkgs.writeText "sway.config" (''
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
      set $term ${pkgs.alacritty}/bin/alacritty

    ##
    ## Output configuration
    ##
      # Default wallpaper
      output * bg ${wallpaper}/default.jpg fill

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
        timeout 300 '${lockCommand}' \
        timeout 600 '${cfg.package}/bin/swaymsg "output * dpms off"' \
             resume '${cfg.package}/bin/swaymsg "output * dpms on"' \
        before-sleep '${lockCommand}'

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
      bindsym $mod+Shift+apostrophe kill

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
      set $ws1 "1"
      set $ws2 "2"
      set $ws3 "3"
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
    # Passthrough mode:
    #
      # These bindings trigger as soon as you enter the resize mode
      mode "passthrough" {
        # Exit passthrough mode:
        bindsym Shift+Escape mode "default"
        bindsym $mod+Shift+r mode "default"
      }

      # Enter passthrough mode:
      bindsym $mod+Shift+r mode "passthrough"

    #
    # Scratchpad:
    #
      # Move window to scratchpad:
      bindsym $mod+Shift+minus move scratchpad

      # Show scratchpad window and cycle through them:
      bindsym $mod+minus scratchpad show

    '' + (lib.optionalString config.my.gaming.enable ''
    #
    # Add keybindings related to the "gaming" module:
    #
      # Add PTT button for mumble:
      bindsym --no-repeat           Super_L exec ${pkgs.glib}/bin/gdbus call -e -d net.sourceforge.mumble.mumble -o / -m net.sourceforge.mumble.Mumble.startTalking
      bindsym --no-repeat --release Super_L exec ${pkgs.glib}/bin/gdbus call -e -d net.sourceforge.mumble.mumble -o / -m net.sourceforge.mumble.Mumble.stopTalking
    '') + ''

    #
    # Other keybindings:
    #
      # Printscreen:
      bindsym Print exec ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.swappy}/bin/swappy -f -

      # Backlight:
      bindsym XF86MonBrightnessUp exec ${pkgs.acpilight}/bin/xbacklight -inc 10
      bindsym XF86MonBrightnessDown exec ${pkgs.acpilight}/bin/xbacklight -dec 10

      # Audio:
      bindsym XF86AudioMute exec ${pactl} set-sink-mute @DEFAULT_SINK@ toggle
      bindsym XF86AudioLowerVolume exec ${pactl} set-sink-volume @DEFAULT_SINK@ -10%
      bindsym XF86AudioRaiseVolume exec ${pactl} set-sink-volume @DEFAULT_SINK@ +10%
      bindsym XF86AudioMicMute exec ${pactl} set-source-mute @DEFAULT_SOURCE@ toggle

      # Misc buttons:
      bindsym XF86Tools exec ${config.services.emacs.package}/bin/emacs
      bindsym XF86Favorites exec ${config.services.emacs.package}/bin/emacs
      # bindsym XF86Display exec pkgs.autorandr/bin/autorandr -cf

      # Rofi emoji picker:
      bindsym $mod+i exec ${rofi}/bin/rofi -show emoji -theme glue_pro_blue

      # Launch screen locker
      bindsym $mod+l exec ${lockCommand}

    ##
    ## Visuals
    ##

      # Set gaps
      gaps inner 5

      # Set fonts
      font "${config.my.fonts.monospace} ${toString config.my.fonts.size}"

      # Set borders instead of title bars for some programs
      for_window [app_id="Alacritty"] border pixel 3
      for_window [app_id="firefox"] border pixel 3
      for_window [app_id="firefox" title="Firefox - Sharing Indicator"] floating enable
      for_window [app_id="firefox" title="Firefox — Sharing Indicator"] floating enable
      for_window [app_id="firefox" title="Picture-in-Picture"] floating enable
      for_window [class="Brave-browser"] border pixel 3
      for_window [class="Chromium-browser"] border pixel 3
      for_window [class="Google-chrome"] border pixel 3
      for_window [title="Welcome to Google Chrome"] floating enable
      for_window [class="Emacs"] border pixel 3
      for_window [app_id="emacs"] border pixel 3

      # Set border instead of title bars when running sway inside of sway
      for_window [app_id="wlroots"] border pixel 3

      # Apply opacity to some programs
      for_window [app_id="Alacritty"] opacity set 0.9
      for_window [class="Emacs"] opacity set 0.99
      for_window [app_id="emacs"] opacity set 0.99

    #
    # Start things:
    #
      # Notification deamon:
      exec ${pkgs.mako}/bin/mako

      # Import variables needed for screen sharing to work.
      exec ${pkgs.systemd}/bin/systemctl --user import-environment XDG_SESSION_TYPE XDG_CURRENT_DESKTOP

      # Import variables needed for screen sharing and gnome3 pinentry to work.
      exec ${pkgs.dbus}/bin/dbus-update-activation-environment WAYLAND_DISPLAY

      # Import variables needed for some other things to work properly.
      exec ${pkgs.systemd}/bin/systemctl --user import-environment WAYLAND_DISPLAY DISPLAY DBUS_SESSION_BUS_ADDRESS SWAYSOCK

    #
    # Status Bar:
    #
    # Read `man 5 sway-bar` for more information about this section.
      bar {
        # Hide the default bar
        mode invisible

        # Run waybar as a bar
        status_command ${pkgs.waybar}/bin/waybar --config ${waybarConfig} --style ${waybarStyles}
      }
  '');

  # Grab the default stylesheet and patch the font family and size.
  waybarStyles = pkgs.runCommandNoCC "waybar-styles.css" { } ''
    sed -e 's/font-family: /font-family: ${config.my.fonts.normal}, /'              \
        -e 's/font-size: 13px/font-size: ${toString config.my.fonts.biggerSize}px/' \
        ${pkgs.waybar}/etc/xdg/waybar/style.css > $out
  '';

  waybarConfig = pkgs.writeText "waybar-config.json" (builtins.toJSON {
    height = 30; # Height of bar

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
  });

in
{
  config = lib.mkIf cfg.enable {
    programs.sway.enable = true;

    # Install fonts needed for waybar
    fonts.fonts = [ pkgs.font-awesome ];

    # Loginmanager
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.autoLogin.enable = true;
    services.xserver.displayManager.autoLogin.user = config.my.user.username;

    # Set up services needed for gnome stuff for evolution
    services.gnome.evolution-data-server.enable = true;
    services.gnome.gnome-keyring.enable = true;

    # Install aditional packages
    environment.systemPackages = with pkgs; [
      evince
      evolution
      gnome3.adwaita-icon-theme # Icons for gnome packages that sometimes use them but don't depend on them
      pavucontrol
      wdisplays
      wlr-randr

      # Scripts to switch between backgrounds
      (writeShellScriptBin "sway-defaultbg" ''
        ${cfg.package}/bin/swaymsg "output * bg ${wallpaper}/default.jpg fill"
      '')
      (writeShellScriptBin "sway-720pfigure" ''
        ${cfg.package}/bin/swaymsg "output * bg ${wallpaper}/720pfigure.jpg fill"
      '')
    ];

    # Configure Firefox to use Wayland
    environment.variables.MOZ_ENABLE_WAYLAND = "1";

    # Set XDG portal related variables
    environment.variables.XDG_SESSION_TYPE = "wayland";
    environment.variables.XDG_CURRENT_DESKTOP = "sway";

    # Run QT programs in wayland mode
    environment.variables.QT_QPA_PLATFORM = "wayland";

    # Set up Pipewire
    services.pipewire.enable = true;

    # Set up XDG Portals
    xdg.portal.enable = true;
    xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];

    # Needed for autologin
    services.xserver.displayManager.defaultSession = "sway";

    environment.etc."sway/config".source = mySwayConfig;
  };
}
