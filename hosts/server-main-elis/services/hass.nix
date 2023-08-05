{
  config,
  pkgs,
  ...
}: {
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
      proxy_read_timeout 300;
      proxy_connect_timeout 300;
      proxy_send_timeout 300;
    '';
  };

  virtualisation.oci-containers.containers = {
    home-assistant = {
      environment.TZ = config.time.timeZone;
      image = "ghcr.io/home-assistant/home-assistant:2023.8.1";
      ports = ["8123"];
      extraOptions = [
        "--network=host"
        "--device=/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2124653-if00:/dev/ttyACM0"
      ];
      volumes = [
        "${config.etu.dataPrefix}/var/lib/hass:/config"
      ];
      dependsOn = ["mqtt" "zwavejs2mqtt"];
    };
    mqtt = {
      image = "eclipse-mosquitto:2.0.15";
      ports = ["1883:1883"];
      extraOptions = [
        "--network=host"
      ];
      volumes = [
        "${config.etu.dataPrefix}/var/lib/mqtt/config:/mosquitto/config:ro"
        "${config.etu.dataPrefix}/var/lib/mqtt/data:/mosquitto/data"
        "${config.etu.dataPrefix}/var/lib/mqtt/log:/mosquitto/log"
      ];
    };
    zwavejs2mqtt = {
      image = "zwavejs/zwavejs2mqtt:8.22.0";
      ports = [
        "3000:3000"
        # "8091:8091" # Admin interface port
      ];
      extraOptions = [
        "--device=/dev/serial/by-id/usb-0658_0200-if00:/dev/zwave"
        "--network=host"
      ];
      volumes = [
        "${config.etu.dataPrefix}/var/lib/zwavejs2mqtt:/usr/src/app/store"
      ];
    };
  };
}
