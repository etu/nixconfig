{ config, pkgs, ... }:

let
  hpkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/0734f95e256632273f8e0220c64351fb43ec0a3e.tar.gz";
      sha256 = "1pjmjpblhkxw2gkfrph53s3460xqxxamqcfqb2rxjrnpkck3rlcf";
    }) { };

in
{
  # Make sure to have NGiNX enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."hass.elis.nu" = {
    forceSSL = true;
    enableACME = true;
    locations."/".proxyWebsockets = true;
    locations."/".proxyPass = "http://127.0.0.1:8123/";
    locations."/".extraConfig = ''
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Host $host;
    '';
  };

  # Enable Home Assistant, open port and add the hass user to the dialout group
  services.home-assistant = {
    enable = true;
    package = hpkgs.home-assistant;
    config = {
      # Basic settings
      homeassistant = {
        name = "Home";
        latitude = "!secret lat_coord";
        longitude = "!secret lon_coord";
        elevation = 22;
        unit_system = "metric";
        time_zone = "Europe/Stockholm";
      };

      # Enable meta default config module that pulls in lots of default
      # modules: https://www.home-assistant.io/integrations/default_config/
      default_config = { };

      # Discover some devices automatically
      discovery = { };

      # Http settings
      http = {
        server_host = "127.0.0.1";
        base_url = "https://hass.elis.nu";
        use_x_forwarded_for = true;
        trusted_proxies = "127.0.0.1";
        server_port = 8123;
      };

      # Include automations
      automation = [
        # Turn on the LED strip and floor lights in the evening
        {
          id = "turn-on-evening-lights";
          alias = "Turn on evening lights";
          trigger = {
            platform = "sun";
            event = "sunset";
            offset = "-00:45:00";
          };
          action = [
            { service = "light.turn_on"; data.entity_id = "light.tv_wall_strip"; }
            { service = "switch.turn_on"; data.entity_id = [ "switch.floorlamp_office" "switch.floorlamp_livingroom" ]; }
          ];
        }

        # Turn off the floor lamps in the evening
        {
          id = "turn-off-evening-switches";
          alias = "Turn off evening switches";
          trigger = [
            { platform = "time"; at = "00:00:00"; }
            {
              platform = "state";
              entity_id = [ "switch.floorlamp_livingroom" "switch.floorlamp_office" ];
              to = "on";
              for.minutes = 30;
            }
          ];
          condition = {
            condition = "time";
            after = "00:00:00";
            before = "10:00:00";
          };
          action.data.entity_id = [ "switch.floorlamp_office" "switch.floorlamp_livingroom" ];
          action.service = "switch.turn_off";
        }

        # Turn off the LED strip in the evening
        {
          id = "turn-off-tv-wall-strip";
          alias = "Turn off TV Wall Strip";
          trigger = { platform = "time"; at = "01:30:00"; };
          action.data.entity_id = "light.tv_wall_strip";
          action.service = "light.turn_off";
        }

        # Turn off hallway ceiling lamps timers
        {
          id = "turn-off-hallway-ceilinglamps";
          alias = "Turn off hallway ceilinglamps";
          trigger = {
            platform = "state";
            entity_id = [ "light.ceilinglamp_hallway_1" "light.ceilinglamp_hallway_2" ];
            for.minutes = 15;
            to = "on";
          };
          action.data.entity_id = [ "light.ceilinglamp_hallway_1" "light.ceilinglamp_hallway_2" ];
          action.service = "light.turn_off";
        }

        # Turn on the other hallway lamp
        {
          id = "turn-on-other-hallway-ceilinglamp";
          alias = "Turn on other hallway ceilinglamp";
          trigger = {
            platform = "state";
            entity_id = [ "light.ceilinglamp_hallway_1" "light.ceilinglamp_hallway_2" ];
            to = "on";
          };
          action.data.entity_id = [ "light.ceilinglamp_hallway_1" "light.ceilinglamp_hallway_2" ];
          action.service = "light.turn_on";
        }

        # Turn on media center power for updates in the evening
        {
          id = "turn-on-media-center-power-for-updates";
          alias = "Turn on media center power for updates";
          trigger = { platform = "time"; at = "00:55:00"; };
          action.data.entity_id = "switch.media_center_power";
          action.service = "switch.turn_on";
        }
        {
          id = "turn-off-media-center-power";
          alias = "Turn off media center power";
          trigger = { platform = "time"; at = "02:30:00"; };
          action.data.entity_id = "switch.media_center_power";
          action.service = "switch.turn_off";
        }

        # Turn off media center when the lazy humans left it on
        {
          id = "turn-off-media-center-to-conserve-power";
          alias = "Turn off media center to conserve power";
          trigger = { platform = "state"; entity_id = "binary_sensor.humans_home"; to = "off"; for.seconds = 30; };
          condition = { condition = "time"; before = "00:55:00"; after = "02:30:00"; };
          action = { service = "switch.turn_off"; data.entity_id = "switch.media_center_power"; };
        }

        # Start the vacuum cleaner if nobody is home or in the evening.
        {
          id = "vacuum-start-cleaning";
          alias = "Vacuum: Start cleaning";
          trigger = [
            { platform = "state"; entity_id = "binary_sensor.humans_home"; to = "off"; for.seconds = 30; }
            { platform = "time"; at = "18:00:00"; }
          ];
          condition = [
            {
              condition = "state";
              entity_id = "input_boolean.vacuum_scheduled_cleaning";
              state = "on";
            }
            {
              condition = "time";
              after = "07:00:00";
              before = "20:30:00";
              # weekday = [ "mon" "tue" "wed" "thu" "fri" "sat" "sun" ];
            }
            {
              condition = "state";
              entity_id = "input_boolean.vacuum_cleaned_today";
              state = "off";
            }
            {
              condition = "state";
              entity_id = "vacuum.jean_luc";
              state = "docked";
            }
          ];
          action.service = "script.vacuum_start_cleaning";
        }

        # Toggle flag when the vacuum is done to not vacuum several times a day
        {
          id = "vacuum-log-return-to-dock";
          alias = "Vacuum: Log return to dock";
          trigger = {
            platform = "state";
            entity_id = "vacuum.jean_luc";
            from = "returning";
            to = "docked";
          };
          action.data.entity_id = "input_boolean.vacuum_cleaned_today";
          action.service = "input_boolean.turn_on";
        }

        # Reset toggle to allow vacuum cleaning the next day
        {
          id = "vacuum-reset-cleaned-today";
          alias = "Vacuum: Reset cleaned today";
          trigger.platform = "time";
          trigger.at = "01:00:00";
          action.data.entity_id = "input_boolean.vacuum_cleaned_today";
          action.service = "input_boolean.turn_off";
        }
      ];

      # Binary sensors
      binary_sensor = [
        {
          platform = "template";
          sensors.humans_home = {
            friendly_name = "Humans home";
            device_class = "presence";
            value_template = "{{ is_state('device_tracker.elis_oneplus_nord', 'home') or is_state('device_tracker.caroline_oneplus_nord', 'home') }}";
          };
        }
      ];

      # Boolean variables for states of things
      input_boolean = {
        vacuum_cleaned_today = {
          name = "Vacuum: Robot cleaned today";
          initial = "off";
        };
        vacuum_scheduled_cleaning = {
          name = "Vacuum: Robot scedule";
          icon = "mdi:calendar-clock";
          initial = "on";
        };
      };

      # Scripts
      script = {
        vacuum_start_cleaning = {
          alias = "Vacuum: Start robot vacuum cleaning";
          sequence = [
            { service = "vacuum.start"; entity_id = "vacuum.jean_luc"; }
            # { service = "vacuum.set_fan_speed"; data = { entity_id = "vacuum.jean_luc"; fan_speed = 75; }; }
          ];
        };
      };

      # ZWave
      zwave = {
        usb_path = "/dev/serial/by-id/usb-0658_0200-if00";
        network_key = "!secret zwave_network_key";
      };

      # ZHA
      zha = {
        # usb_path = "/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2124653-if00";
        # radio_type = "deconz";
        database_path = "/var/lib/hass/zigbee.db";
        enable_quirks = false;
      };

      # Make the ui configurable through ui-lovelace.yaml
      lovelace.mode = "yaml";
      lovelace.resources = [
        { url = "/local/vacuum-card.js";  type = "module"; }
      ];

      # Purge tracked history after 10 days
      recorder.purge_keep_days = 10;

      # Automatic chromecast detection
      cast = [
        { media_player = { }; }
      ];

      # Media players
      media_player = [
        { platform = "kodi"; host = "kodi.lan"; }
      ];

      # Enable logging
      logger.default = "info";

      # Enable vacuum cleaner
      vacuum = [
        {
          name = "Jean-Luc";
          platform = "xiaomi_miio";
          host = "!secret vacuum_host";
          token = "!secret vacuum_token";
        }
      ];

      # Pull in weather data
      weather = [
        {
          platform = "openweathermap";
          api_key = "!secret openweathermap_api_key";
        }
      ];
    };
  };

  users.users.hass.extraGroups = [ "dialout" ];

  # Bind mount home assistants files to have persistence of hass configs
  fileSystems."/var/lib/hass" = {
    device = "/persistent/var/lib/hass";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };
}
