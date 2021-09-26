{ config, pkgs, ... }:

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

  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers.home-assistant = {
    autoStart = false;
    environment.TZ = config.time.timeZone;
    image = "ghcr.io/home-assistant/home-assistant:2021.9.7";
    ports = [ "8123" ];
    extraOptions = [
      "--privileged"
      "--net=host"
      "--device=/dev/ttyACM0:/dev/ttyACM0"
      "--device=/dev/ttyACM1:/dev/ttyACM1"
    ];
    volumes = [
      "/persistent/var/lib/hass:/config"
    ];
  };
}
