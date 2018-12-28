{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.i3;
  myI3wmPkg = pkgs.i3;
  myI3Config = pkgs.writeText "i3wm.config" ''
    # i3 config file (v4)
    #
    # Please see https://i3wm.org/docs/userguide.html for a complete reference!

    set $mod Mod4

    # Font for window titles. Will also be used by the bar unless a different font
    # is used in the bar {} block below.
    font pango:monospace 8

    # Use Mouse+$mod to drag floating windows to their wanted position
    floating_modifier $mod

    # Start a terminal
    bindsym $mod+Return exec ${myI3wmPkg}/bin/i3-sensible-terminal

    # Kill focused window
    bindsym $mod+Shift+aring kill

    # Start rofi (to launch and select programs)
    bindsym $mod+e exec ${pkgs.rofi}/bin/rofi -show combi -theme glue_pro_blue

    # Change focus (Emacs style)
    bindsym $mod+b focus left
    bindsym $mod+f focus right
    bindsym $mod+p focus up
    bindsym $mod+n focus down

    # Alternatively, you can use the cursor keys:
    bindsym $mod+Left focus left
    bindsym $mod+Down focus down
    bindsym $mod+Up focus up
    bindsym $mod+Right focus right

    # Move focused window (Emacs style)
    bindsym $mod+Shift+b move left
    bindsym $mod+Shift+f move right
    bindsym $mod+Shift+p move up
    bindsym $mod+Shift+n move down

    # Alternatively, you can use the cursor keys:
    bindsym $mod+Shift+Left move left
    bindsym $mod+Shift+Down move down
    bindsym $mod+Shift+Up move up
    bindsym $mod+Shift+Right move right

    # Split in horizontal orientation
    bindsym $mod+h split h

    # Split in vertical orientation
    bindsym $mod+v split v

    # Fullscreen for the focused container
    bindsym $mod+u fullscreen toggle

    # Change container layout (stacked, tabbed, toggle split)
    bindsym $mod+o layout stacking
    bindsym $mod+adiaeresis layout tabbed
    bindsym $mod+odiaeresis layout toggle split

    # Toggle tiling / floating
    bindsym $mod+Shift+space floating toggle

    # Change focus between tiling / floating windows
    bindsym $mod+space focus mode_toggle

    # Focus the parent container
    bindsym $mod+a focus parent

    # Focus the child container
    bindsym $mod+d focus child

    # Define names for default workspaces for which we configure key bindings later on.
    # We use variables to avoid repeating the names in multiple places.
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

    # Switch to workspace
    bindsym $mod+1 workspace $ws1
    bindsym $mod+2 workspace $ws2
    bindsym $mod+3 workspace $ws3
    bindsym $mod+4 workspace $ws4
    bindsym $mod+5 workspace $ws5
    bindsym $mod+6 workspace $ws6
    bindsym $mod+7 workspace $ws7
    bindsym $mod+8 workspace $ws8
    bindsym $mod+9 workspace $ws9
    bindsym $mod+0 workspace $ws10

    # Move focused container to workspace
    bindsym $mod+Shift+1 move container to workspace $ws1
    bindsym $mod+Shift+2 move container to workspace $ws2
    bindsym $mod+Shift+3 move container to workspace $ws3
    bindsym $mod+Shift+4 move container to workspace $ws4
    bindsym $mod+Shift+5 move container to workspace $ws5
    bindsym $mod+Shift+6 move container to workspace $ws6
    bindsym $mod+Shift+7 move container to workspace $ws7
    bindsym $mod+Shift+8 move container to workspace $ws8
    bindsym $mod+Shift+9 move container to workspace $ws9
    bindsym $mod+Shift+0 move container to workspace $ws10

    # Reload the configuration file
    bindsym $mod+Shift+j reload

    # Restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
    bindsym $mod+Shift+l restart

    # Exit i3 (logs you out of your X session)
    bindsym $mod+Shift+odiaeresis exec "${myI3wmPkg}/bin/i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' '${myI3wmPkg}/bin/i3-msg exit'"

    # Resize window (you can also use the mouse for that)
    mode "resize" {
      # These bindings trigger as soon as you enter the resize mode

      # Pressing left will shrink the window’s width.
      # Pressing right will grow the window’s width.
      # Pressing up will shrink the window’s height.
      # Pressing down will grow the window’s height.
      bindsym b resize shrink width 10 px or 10 ppt
      bindsym f resize grow width 10 px or 10 ppt
      bindsym n resize grow height 10 px or 10 ppt
      bindsym p resize shrink height 10 px or 10 ppt

      # same bindings, but for the arrow keys
      bindsym Left resize shrink width 10 px or 10 ppt
      bindsym Down resize grow height 10 px or 10 ppt
      bindsym Up resize shrink height 10 px or 10 ppt
      bindsym Right resize grow width 10 px or 10 ppt

      # back to normal: Enter or Escape or $mod+r
      bindsym Return mode "default"
      bindsym Escape mode "default"
      bindsym $mod+r mode "default"
    }

    bindsym $mod+r mode "resize"

    bindsym $mod+l exec ${pkgs.i3lock}/bin/i3lock -n -c 000000

    # Start i3bar to display a workspace bar (plus the system information i3status
    # finds out, if available)
    bar {
      status_command i3status
      tray_output primary
      position top
      font pango:monospace 10
    }
  '';

in {
  options = {
    my.i3 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enables i3wm and auto login for my user
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # Libinput
    services.xserver.libinput.enable = true;

    # Loginmanager
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.autoLogin.enable = true;
    services.xserver.displayManager.lightdm.autoLogin.user = config.my.user.username;

    # Needed for autologin
    services.xserver.desktopManager.default = "none";
    services.xserver.windowManager.default = "i3";

    # Set up i3
    services.xserver.windowManager.i3.enable = true;
    services.xserver.windowManager.i3.package = myI3wmPkg;
    services.xserver.windowManager.i3.configFile = myI3Config;
    services.xserver.windowManager.i3.extraPackages = with pkgs; [
      dmenu
      i3status
      i3lock
      pavucontrol
      gnome3.networkmanagerapplet
    ];

    # Set up services needed for gnome stuff for evolution
    services.gnome3.evolution-data-server.enable = true;
    services.gnome3.gnome-keyring.enable = true;

    # Install aditional packages
    environment.systemPackages = with pkgs; [
      arandr
      evince
      gnome3.evolution
      scrot
    ];

    # Configure TERMINAL for i3-sensible-terminal
    environment.variables.TERMINAL = "stupidterm";

    # Enable i3lock on suspend
    systemd.services.i3lock = {
      description = "Lock screen before suspend";
      before = [ "sleep.target" ];
      wantedBy = [ "suspend.target" ];

      serviceConfig = {
        User = config.my.user.username;
        Type = "simple";
        Environment = "DISPLAY=:0";
        ExecStart = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
        ExecStartPost = "${pkgs.coreutils}/bin/sleep 1";
      };
    };
  };
}
