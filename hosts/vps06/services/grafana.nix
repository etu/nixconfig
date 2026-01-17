{
  config,
  pkgs,
  ...
}:
{
  etu.base.zfs.system.directories = [
    # Bind mount for persistent data for grafana
    config.services.grafana.dataDir
  ];

  # Enable grafana
  services.grafana.enable = true;
  services.grafana.declarativePlugins = [
    pkgs.grafanaPlugins.yesoreyeram-infinity-datasource
  ];

  services.nginx.virtualHosts."grafana.elis.nu" = {
    forceSSL = true;
    enableACME = true;
    locations."/".proxyPass = "http://localhost:3000/";
    locations."/".extraConfig = ''
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Host $host;
    '';
  };
}
