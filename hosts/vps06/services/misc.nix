{pkgs, ...}: {
  # Enable the ip-failar-nu service
  services.ip-failar-nu.enable = true;

  # This is to avoid nginx validation errors when upgrading tailscale
  # and restarting nginx at the same time.
  networking.hosts."100.100.6.114" = ["server-main-elis"];

  services.nginx.virtualHosts = {
    "ip.failar.nu" = {
      addSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:8123/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };
    "sa.0b.se" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "https://elis.nu/";
    };
    "sa4b.se" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "https://elis.nu/";
    };
    "freshrss.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://server-main-elis/";
      locations."/".extraConfig = "proxy_set_header Host $host;";
    };
    "nextcloud.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://server-main-elis/";
      locations."/".extraConfig = "proxy_set_header Host $host;";
    };
    "hass.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyWebsockets = true;
      locations."/".proxyPass = "http://server-main-elis:8123/";
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
    "jellyfin.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://server-main-elis:8096";
        proxyWebsockets = true;
      };
    };
    "misc.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://server-main-elis:80";
      locations."/".extraConfig = "proxy_set_header Host $host;";
      locations."/robots.txt".root = pkgs.writeTextDir "robots.txt" ''
        User-agent: *
        Disallow: /
      '';
    };
  };

  services.nginx.clientMaxBodySize = "20m"; # Increase body size since we handle jellyfin.

  # Configure temp paths
  services.nginx.appendHttpConfig = ''
    proxy_temp_path       /var/cache/nginx/proxy_temp 1 2;
    client_body_temp_path /var/cache/nginx/client_temp 1 2;
  '';

  # Ensure directories exist with correct perms
  systemd.tmpfiles.rules = [
    "d /var/cache/nginx 0755 nginx nginx -"
    "d /var/cache/nginx/proxy_temp 0750 nginx nginx -"
    "d /var/cache/nginx/client_temp 0750 nginx nginx -"
  ];
}
