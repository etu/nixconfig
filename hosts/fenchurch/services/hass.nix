{ config, pkgs, ... }:

let
  hpkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/39e6bf76474ce742eb027a88c4da6331f0a1526f.tar.gz";
      sha256 = "1pxigbywdq4yf7smas6zq4vhakbkvm1vhj443qjikh77fc8hy17b";
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
            { service = "switch.turn_on"; data.entity_id = [ "switch.floorlamp_office" "switch.floorlamp_bookshelf" ]; }
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
              entity_id = [ "switch.floorlamp_office" "switch.floorlamp_bookshelf" ];
              to = "on";
              for.minutes = 30;
            }
          ];
          condition = {
            condition = "time";
            after = "00:00:00";
            before = "10:00:00";
          };
          action.data.entity_id = [ "switch.floorlamp_office" "switch.floorlamp_bookshelf" ];
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

        # Turn on media center power for updates in the evening
        {
          id = "turn-on-media-center-power-for-updates";
          alias = "Turn on media center power for updates";
          trigger = { platform = "time"; at = "23:59:00"; };
          condition = { condition = "time"; weekday = [ "sun" ]; };
          action.data.entity_id = "switch.media_center_power";
          action.service = "switch.turn_on";
        }
        {
          id = "turn-off-media-center-power";
          alias = "Turn off media center power";
          trigger = { platform = "time"; at = "00:30:00"; };
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
          initial = "on";
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
    }; # END config = {

    lovelaceConfig = {
      title = "Home";
      views = [
        {
          title = "Home";
          icon = "mdi:information-outline";
          panel = false;
          path = "home";
          # Show badges on top
          badges = [
            { entity = "binary_sensor.humans_home"; }
            { entity = "person.elis_hirwing"; }
            { entity = "person.caroline_hirwing"; }
            { entity = "sensor.livingroom_temperature"; }
            { entity = "sensor.livingroom_humidity"; }
          ];
          cards = [
            # Panel with all lamps
            {
              type = "entities";
              title = "Lights";
              show_header_toggle = true;
              entities = [
                { entity = "light.tv_wall_strip"; icon = "mdi:led-strip"; }
                { entity = "switch.floorlamp_office"; icon = "mdi:floor-lamp"; }
                { entity = "switch.floorlamp_bookshelf"; icon = "mdi:floor-lamp"; }
              ];
            }
            # Panel with settings
            {
              type = "entities";
              title = "Settings";
              show_header_toggle = false;
              entities = [
                { entity = "input_boolean.vacuum_cleaned_today"; }
                { entity = "input_boolean.vacuum_scheduled_cleaning"; }
                { entity = "switch.media_center_power"; }
              ];
            }
            # Weather forecast
            { type = "weather-forecast"; entity = "weather.openweathermap"; }
            # Kodi status
            { type = "media-control"; entity = "media_player.kodi"; }
            # Vacuum cleaner
            {
              type = "custom:vacuum-card";
              entity = "vacuum.jean_luc";
              stats.default = [
                { attribute = "filter_left"; unit = "hours"; subtitle = "Filter"; }
                { attribute = "side_brush_left"; unit = "hours"; subtitle = "Side brunch"; }
                { attribute = "main_brush_left"; unit = "hours"; subtitle = "Main brunsh"; }
                { attribute = "sensor_dirty_left"; unit = "hours"; subtitle = "Sensors"; }
              ];
              stats.cleaning = [
                { attribute = "cleaned_area"; unit = "m2"; subtitle = "Cleaning area"; }
                { attribute = "cleaning_time"; unit = "minutes"; subtitle = "Cleaning time"; }
              ];
            }
          ];
        }
        {
          title = "Floorplan";
          icon = "mdi:map";
          panel = true;
          path = "floorplan";
          cards = [
            {
              type = "picture-elements";
              title = "Floorplan";
              image = "/local/img/floorplan.png";
              elements = [
                # Add floorlamp in the livingroom to the floorplan
                {
                  type = "state-icon";
                  entity = "switch.floorlamp_bookshelf";
                  title = "Floorlamp Livingroom";
                  icon = "mdi:floor-lamp";
                  tap_action.action = "toggle";
                  style = { top = "50%"; left = "70%"; };
                }
                # Add floorlamp in the office to the floorplan
                {
                  type = "state-icon";
                  entity = "switch.floorlamp_office";
                  title = "Floorlamp Office";
                  icon = "mdi:floor-lamp";
                  tap_action.action = "toggle";
                  style = { top = "48%"; left = "54%"; };
                }
                # Add Hue lightstrip in the livingroom to the floorplan
                {
                  type = "state-icon";
                  entity = "light.tv_wall_strip";
                  title = "Hue Lightstrip";
                  icon = "mdi:led-strip";
                  tap_action.action = "toggle";
                  style = { top = "52%"; left = "87%"; };
                }
              ];
            }
          ];
        }
      ];
    }; # END lovelaceConfig  = {
  };

  users.users.hass.extraGroups = [ "dialout" ];

  # Bind mount home assistants files to have persistence of hass configs
  fileSystems."/var/lib/hass" = {
    device = "/persistent/var/lib/hass";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };
}
