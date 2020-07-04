{ config, pkgs, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "matomo.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:18889/";
      locations."/".extraConfig = ''
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Host $host;
      '';
    };
  };

  containers.matomo = {
    bindMounts = {
      "matomo-data" = {
        mountPoint = "/var/lib/matomo/config";
        hostPath = "/persistent/var/lib/matomo";
        isReadOnly = false;
      };
      "mysql-data" = {
        mountPoint = "/var/lib/mysql";
        hostPath = "/persistent/var/lib/matomo-mysql";
        isReadOnly = false;
      };
    };

    config = { config, pkgs, lib, ... }: {
      services.matomo.enable = true;
      services.matomo.nginx = {
        forceSSL = false;
        enableACME = false;

        listen = [{ addr = "0.0.0.0"; port = 18889; }];
      };

      systemd.services.matomo-archive-processing.serviceConfig.ExecStart = lib.mkForce
        "${config.services.matomo.package}/bin/matomo-console core:archive --url=https://matomo.elis.nu";

      services.nginx.enable = true;
      services.mysql.enable = true;
      services.mysql.package = pkgs.mariadb;
    };

    # end container
  };
}
