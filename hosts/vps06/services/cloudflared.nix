{ config, lib, ... }:
let
  # UUID printed by `cloudflared tunnel create vps06`.
  tunnelId = "0c1fc03d-e863-43cb-8ee3-2d6944a62695";

  # Hostnames served through the tunnel. Each one is an nginx virtual host
  # on this machine and gets a proxied CNAME to <tunnelId>.cfargotunnel.com.
  hostnames = [
    "git.elis.nu"
    "matomo.elis.nu"
  ];
in
{
  age.secrets.cloudflared-elis-nu-cert-pem = config.etu.data.ageModules.cloudflared-elis-nu-cert-pem;
  age.secrets.cloudflared-vps06-tunnel = config.etu.data.ageModules.cloudflared-vps06-tunnel;

  services.cloudflared = {
    enable = true;
    # Account certificate at /etc/cloudflared/cert.pem, so `cloudflared
    # tunnel …` management commands work as root on this host.
    certificateFile = config.age.secrets.cloudflared-elis-nu-cert-pem.path;

    tunnels.${tunnelId} = {
      credentialsFile = config.age.secrets.cloudflared-vps06-tunnel.path;
      ingress = lib.genAttrs hostnames (_: "http://localhost:80");
      default = "http_status:404";
    };
  };

  # Cloudflare terminates TLS, so these virtual hosts are plain HTTP and only
  # listen on loopback, where cloudflared connects to them. They can't be
  # reached directly through the public ports 80/443, which the failar.nu
  # sites still use.
  services.nginx.virtualHosts = lib.genAttrs hostnames (_: {
    listen = [
      {
        addr = "127.0.0.1";
        port = 80;
      }
      {
        addr = "[::1]";
        port = 80;
      }
    ];

    # Log and pass on the visitor's address instead of cloudflared's.
    extraConfig = ''
      set_real_ip_from 127.0.0.1;
      set_real_ip_from ::1;
      real_ip_header CF-Connecting-IP;
    '';
  });
}
