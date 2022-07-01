{ config, pkgs, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.clientMaxBodySize = "20m"; # Increase body size since we handle video.
  services.nginx.virtualHosts = {
    "jellyfin.elis.nu" = let
      locationConfig = {
        proxyWebsockets = true;
        proxyPass = "http://127.0.0.1:8096/";
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
          proxy_set_header X-Forwarded-Protocol $scheme;
          proxy_set_header X-Forwarded-Host $http_host;

          # Disable buffering when the nginx proxy gets very resource heavy upon streaming
          proxy_buffering off;
      '';
      };
    in {
      forceSSL = true;
      enableACME = true;
      locations."/" = locationConfig;
      locations."= /web/" = locationConfig;
      locations."/socket" = locationConfig;
    };
  };

  # Bind mount for persistent data for jellyfin
  fileSystems."/var/lib/jellyfin" = {
    device = "/persistent/var/lib/jellyfin";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Enable jellyfin itself
  services.jellyfin.enable = true;

  # Open NGiNX port
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
