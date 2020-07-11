{ config, pkgs, ... }:
let
  hpkgs = import
    (builtins.fetchTarball {
      url = https://github.com/NixOS/nixpkgs/archive/f7334cad427d8eb80a51247a443cfc85514fdba0.tar.gz;
    }) { };
in
{
  # Make sure to have NGiNX enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."hass.lan".locations."/" = {
    proxyWebsockets = true;
    proxyPass = "http://127.0.0.1:8123";
    extraConfig = ''
      allow 10.3.0.0/24;
      deny all;
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

      # Discover some devices automatically
      discovery = { };

      # Show some system health data
      system_health = { };

      # Http settings
      http = {
        server_host = "127.0.0.1";
        base_url = "http://hass.lan";
        use_x_forwarded_for = true;
        trusted_proxies = "127.0.0.1";
        server_port = 8123;
      };

      # Enables a map showing the location of tracked devies
      map = { };

      # Track the sun
      sun = { };

      # Include automations
      automation = "!include automations.yaml";

      # ZWave
      zwave = {
        usb_path = "/dev/serial/by-id/usb-0658_0200-if00";
        network_key = "!secret zwave_network_key";
      };

      # ZHA
      zha = {
        usb_path = "/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2124653-if00";
        radio_type = "deconz";
        database_path = "/var/lib/hass/zigbee.db";
        enable_quirks = false;
      };

      # Enable mobile app
      mobile_app = { };

      # Enable configuration UI
      config = { };

      # Make the ui configurable through ui-lovelace.yaml
      lovelace.mode = "yaml";

      # Enable support for tracking state changes over time
      history = { };

      # Purge tracked history after 10 days
      recorder.purge_keep_days = 10;

      # View all events in o logbook
      logbook = { };

      # Enable logging
      logger = {
        default = "info";
        logs = {
          "bellows.ezsp" = "debug";
          "bellows.zigbee.application" = "debug";
          "homeassistant.components.zha" = "debug";
          "homeassistant.core" = "debug";
          "mobile_app" = "debug";
          "zhaquirks" = "debug";
          "zigpy" = "debug";
          "zigpy_cc" = "debug";
          "zigpy_deconz.api" = "debug";
          "zigpy_deconz.zigbee.application" = "debug";
          "zigpy_xbee.api" = "debug";
          "zigpy_xbee.zigbee.application" = "debug";
          "zigpy_zigate" = "debug";
        };
      };

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
