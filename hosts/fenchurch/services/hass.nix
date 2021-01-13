{ config, pkgs, ... }:

let
  hpkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/257cbbcd3ab7bd96f5d24d50adc807de7c82e06d.tar.gz";
      sha256 = "0g3n725kjk2fc9yn9rvdjwci4mrx58yrdgp3waby9ky3d5xhcaw4";
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
        # usb_path = "/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2124653-if00";
        # radio_type = "deconz";
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
