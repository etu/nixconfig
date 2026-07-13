{ config, ... }:
{
  age.secrets.cloudflared-elis-nu-cert-pem = config.etu.data.ageModules.cloudflared-elis-nu-cert-pem;
  age.secrets.cloudflared-elis-nu-tunnel = config.etu.data.ageModules.cloudflared-elis-nu-tunnel;

  services.cloudflared = {
    enable = true;
    certificateFile = config.age.secrets.cloudflared-elis-nu-cert-pem.path;

    tunnels."77c4d618-a4c8-490f-93f4-de65809380c6" = {
      credentialsFile = config.age.secrets.cloudflared-elis-nu-tunnel.path;
      ingress = {
        "hass.elis.nu" = {
          service = "http://localhost:8123";
        };
        "misc.elis.nu" = {
          service = "http://localhost:80";
        };
        "nextcloud.elis.nu" = {
          service = "http://localhost:80";
        };
        "freshrss.elis.nu" = {
          service = "http://localhost:80";
        };
        "jellyfin.elis.nu" = {
          service = "http://localhost:8096";
        };
      };
      default = "http_status:404";
    };
  };
}
