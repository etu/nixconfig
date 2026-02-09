{
  config,
  ...
}:
{
  # Garbage collect podman images
  virtualisation.podman.autoPrune.enable = true;
  virtualisation.podman.autoPrune.flags = [ "--all" ];

  virtualisation.oci-containers.containers = {
    home-assistant = {
      environment.TZ = config.time.timeZone;
      image = "ghcr.io/home-assistant/home-assistant:2026.2.1";
      ports = [ "8123" ];
      extraOptions = [
        "--network=host"
        "--device=/dev/serial/by-id/usb-Silicon_Labs_CP2102N_USB_to_UART_Bridge_Controller_041c5694bebaed119e51ad8238a92db5-if00-port0:/dev/ttyUSB0"
        "--device=/dev/serial/by-id/usb-Nabu_Casa_SkyConnect_v1.0_46eea243d3b3ed11bf5a46aca7669f5d-if00-port0:/dev/ttyUSB1"
      ];
      volumes = [
        "${config.etu.dataPrefix}/var/lib/hass:/config"
        "/media/zstorage/files/video/svt-series:/media:ro"
      ];
      dependsOn = [
        "mqtt"
        "zwavejs2mqtt"
      ];
    };
    mqtt = {
      image = "eclipse-mosquitto:2.0.22";
      ports = [ "1883:1883" ];
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
      image = "zwavejs/zwavejs2mqtt:11.11.0";
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
