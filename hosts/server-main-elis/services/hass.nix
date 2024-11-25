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

  # Garbage collect podman images
  systemd.services.podman-system-prune = {
    description = "Garbage collect podman";
    after = ["podman.service"];
    wantedBy = ["multi-user.target"];
    startAt = "05:30";
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      Group = "root";
      ExecStart = "${pkgs.podman}/bin/podman system prune -a -f";
    };
  };

  virtualisation.oci-containers.containers = {
    home-assistant = {
      environment.TZ = config.time.timeZone;
      image = "ghcr.io/home-assistant/home-assistant:2024.11.3";
      ports = ["8123"];
      extraOptions = [
        "--network=host"
        "--device=/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2124653-if00:/dev/ttyACM0"
        "--device=/dev/serial/by-id/usb-Nabu_Casa_SkyConnect_v1.0_46eea243d3b3ed11bf5a46aca7669f5d-if00-port0:/dev/ttyZigbee"
        "--device=/dev/serial/by-id/usb-Silicon_Labs_CP2102N_USB_to_UART_Bridge_Controller_041c5694bebaed119e51ad8238a92db5-if00-port0:/dev/ttyUSB0"
      ];
      volumes = [
        "${config.etu.dataPrefix}/var/lib/hass:/config"
      ];
      dependsOn = ["mqtt" "zwavejs2mqtt"];
    };
    mqtt = {
      image = "eclipse-mosquitto:2.0.20";
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
      image = "zwavejs/zwavejs2mqtt:9.27.7";
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
