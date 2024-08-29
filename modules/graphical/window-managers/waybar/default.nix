{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.window-managers.waybar.enable = lib.mkEnableOption "Enable waybar, a bar for wayland";

  config = lib.mkIf config.etu.graphical.window-managers.waybar.enable {
    # Install fonts needed for waybar
    fonts.packages = [pkgs.font-awesome];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Configure waybar
      programs.waybar = {
        enable = true;
        systemd.enable = true;
        systemd.target = "sway-session.target";
        style = pkgs.runCommand "waybar-styles.css" {} ''
          sed -e 's/font-family: /font-family: ${config.etu.graphical.theme.fonts.normal}, /'              \
              -e 's/font-size: 13px/font-size: ${toString config.etu.graphical.theme.fonts.biggerSize}px/' \
              ${./style.css} > $out
        '';
        settings = [
          {
            # Height of bar
            height = 30;

            # Margins for bar
            margin-top = 0;
            margin-bottom = 0;
            margin-right = 100;
            margin-left = 100;

            modules-left = ["idle_inhibitor" "backlight" "cpu" "memory" "temperature" "battery" "mpris"];
            modules-center = ["sway/workspaces" "sway/mode" "sway/scratchpad"];
            modules-right = ["privacy" "pulseaudio" "network" "clock" "tray"];

            "sway/workspaces" = {
              disable-scroll = true;
              format = "{icon}";
              format-icons = {
                "1" = "Ⅰ";
                "2" = "Ⅱ";
                "3" = "Ⅲ";
                "4" = "Ⅳ";
                "5" = "Ⅴ";
                "6" = "Ⅵ";
                "7" = "Ⅶ";
                "8" = "Ⅷ";
                "9" = "Ⅸ";
                "10" = "Ⅹ";
                default = "";
              };
            };

            "sway/scratchpad" = {
              format = "{icon} {count}";
              format-icons = ["" ""];
              show-empty = false;
              tooltip = true;
              tooltip-format = "{app}: {title}";
            };

            "sway/mode".format = "<span style=\"italic\">{}</span>";

            mpris = {
              format = "{player_icon} {status_icon} {dynamic}";
              format-paused = "{player_icon} {status_icon} <i>{dynamic}</i>";
              tooltip-format = "{player_icon} {status_icon} {dynamic}";
              tooltip-format-paused = "{player_icon} {status_icon} {dynamic}";
              artist-len = 15;
              album-len = 0;
              title-len = 30;
              dynamic-len = 40;
              player-icons = {
                default = "";
                firefox = "";
              };
              status-icons = {
                default = "▶";
                paused = "⏸";
              };
            };

            backlight.format = "{percent}% {icon}";
            backlight.format-icons = ["" ""];

            battery.format = "{capacity}% {icon}";
            battery.format-alt = "{time} {icon}";
            battery.format-charging = "{capacity}% ";
            battery.format-full = "{capacity}% {icon}";
            battery.format-good = "{capacity}% {icon}";
            battery.format-icons = ["" "" "" "" ""];
            battery.format-plugged = "{capacity}% ";
            battery.states = {
              good = 80;
              warning = 30;
              critical = 15;
            };

            clock.format = "<span color=\"#88c0d0\"></span> {:%Y-%m-%d %H:%M:%S}";
            clock.interval = 5;
            clock.tooltip = false;

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
              default = ["" "" ""];
            };
            pulseaudio.on-click = "${pkgs.pavucontrol}/bin/pavucontrol";

            privacy = {
              icon-spacing = 4;
              icon-size = 18;
              transition-duration = 250;
              modules = [
                {
                  type = "screenshare";
                  tooltip = true;
                  tooltip-icon-size = 24;
                }
                {
                  type = "audio-out";
                  tooltip = true;
                  tooltip-icon-size = 24;
                }
                {
                  type = "audio-in";
                  tooltip = true;
                  tooltip-icon-size = 24;
                }
              ];
            };

            temperature.critical-threshold = 80;
            temperature.format = "{icon} {temperatureC}°C";
            temperature.format-icons = ["" "" ""];

            tray.spacing = 12;
          }
        ];
      }; # END waybar
    };
  };
}
