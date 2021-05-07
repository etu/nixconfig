{ config, pkgs, ... }:

{
  # Make sure to have NGiNX enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."netdata.lan".locations."/" = {
    proxyPass = "http://127.0.0.1:19999";
    extraConfig = ''
      allow 192.168.0.0/24;
      deny all;
    '';
  };

  # Enable Netdata
  services.netdata.enable = true;
}
