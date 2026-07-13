{
  flake,
  ...
}:
{
  imports = [
    # Import the ip-failar-nu module
    flake.inputs.ip-failar-nu.nixosModules.x86_64-linux.default
  ];

  # Enable the ip-failar-nu service
  services.ip-failar-nu.enable = true;

  services.nginx.virtualHosts = {
    "ip.failar.nu" = {
      addSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:8123/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };
  };

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
