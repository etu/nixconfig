{ config, pkgs, ... }:

{
  # Make sure to have NGiNX enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."netdata.lan" = {
    locations."/".extraConfig = ''
      allow 10.3.0.0/24;
      deny all;

      proxy_pass http://127.0.0.1:19999;
      proxy_http_version 1.1;
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;
    '';
  };

  # Enable Netdata
  services.netdata.enable = true;
}
