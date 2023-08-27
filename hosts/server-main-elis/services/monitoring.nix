{
  config,
  myData,
  pkgs,
  ...
}: {
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."grafana.elis.nu" = {
    forceSSL = true;
    enableACME = true;
    locations."/".proxyWebsockets = true;
    locations."/".proxyPass = "http://127.0.0.1:3030/";
    locations."/".extraConfig = ''
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    '';
  };

  # Enable grafana.
  services.grafana.enable = true;
  services.grafana.settings.server.http_addr = "0.0.0.0";
  services.grafana.settings.server.http_port = 3030;
  services.grafana.settings.server.domain = "grafana.elis.nu";

  # Set an admin password.
  services.grafana.settings.security.admin_user = "admin";
  services.grafana.settings.security.admin_password = "$__file{${config.age.secrets.grafana-admin-password.path}}";

  # Decrypt secret to expected location.
  age.secrets = {
    inherit (myData.ageModules) grafana-admin-password;
  };

  networking.firewall.allowedTCPPorts = [3030];

  etu.base.zfs.system.directories = [
    # Bind mount for persistent database for grafana
    "/var/lib/grafana"
  ];
}
