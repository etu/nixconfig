{ config, pkgs, ... }:

{
  # Make sure to have NGiNX enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."netdata.lan" = {
    listen = [ { addr = "0.0.0.0"; port = 81; } ];
    locations."/".extraConfig = ''
      proxy_pass http://127.0.0.1:19999;
      proxy_http_version 1.1;
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;
    '';
  };

  # Open NGiNX port
  networking.firewall.allowedTCPPorts = [ 81 ];

  # Enable Netdata
  services.netdata.enable = true;
}
