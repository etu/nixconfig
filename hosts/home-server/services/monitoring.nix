{ config, pkgs, ... }:

let
  # Import age secrets paths and metadata.
  ageModules = (import ../../../data.nix).ageModules;

in {
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


  # Enable prometheus to gather metrics.
  services.prometheus.enable = true;

  # Enable some exporters to gather metrics from.
  services.prometheus.exporters.node.enable = true;
  services.prometheus.exporters.apcupsd.enable = true;
  services.prometheus.exporters.systemd.enable = true;

  # Create a service for the ZFS exporter. It defaults to listen to :9134
  systemd.services.zfs-exporter = {
    description = "ZFS Prometheus exporter";
    after = [ "network.target" "network-online.target" ];
    before = [ "prometheus.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [ config.boot.zfs.package ];
    serviceConfig = {
      ExecStart = "${pkgs.prometheus-zfs-exporter}/bin/zfs_exporter --properties.dataset-filesystem=available,compressratio,logicalused,quota,referenced,used,usedbydataset,written";
      Restart = "always";
    };
  };

  virtualisation.oci-containers.containers = {
    nzbget-exporter = {
      image = "frebib/nzbget-exporter:0.2.2";
      environment = {
        NZBGET_HOST = "http://local.elis.nu/nzbget";
        NZBGET_USERNAME = "";
        NZBGET_PASSWORD = "";
      };
      extraOptions = [ "--network=host" ];
      ports = [ "9452" ];
    };
  };

  # Configure prometheus to gather the data.
  services.prometheus.scrapeConfigs = [
    {
      job_name = "prometheus";
      static_configs = [{ targets = [ "127.0.0.1:${toString config.services.prometheus.port}" ]; }];
    }
    {
      job_name = "node";
      static_configs = [{ targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ]; }];
    }
    {
      job_name = "apcupsd";
      static_configs = [{ targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.apcupsd.port}" ]; }];
    }
    {
      job_name = "systemd";
      static_configs = [{ targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.systemd.port}" ]; }];
    }
    {
      job_name = "grafana";
      scheme = "https";
      static_configs = [{ targets = [ "grafana.elis.nu" ]; }];
    }
    {
      job_name = "zfs_exporter";
      static_configs = [{ targets = [ "127.0.0.1:9134" ]; }];
    }
    {
      job_name = "nzbget";
      static_configs = [{ targets = [ "127.0.0.1:9452" ]; }];
    }
  ];


  # Enable grafana.
  services.grafana.enable = true;
  services.grafana.settings.server.http_addr = "0.0.0.0";
  services.grafana.settings.server.http_port = 3030;
  services.grafana.settings.server.domain = "grafana.elis.nu";

  # Set an admin password.
  services.grafana.settings.security.admin_user = "admin";
  services.grafana.settings.security.admin_password = "$__file{${config.age.secrets.grafana-admin-password.path}}";

  # Provision datasources for me.
  services.grafana.provision.enable = true;
  services.grafana.provision.datasources.settings = {
    apiVersion = 1;
    datasources = [
      {
        isDefault = true;
        name = "Prometheus";
        type = "prometheus";
        url = "http://127.0.0.1:${toString config.services.prometheus.port}";
      }
    ];
  };

  # Decrypt secret to expected location.
  age.secrets = {
    inherit (ageModules) grafana-admin-password;
  };

  networking.firewall.allowedTCPPorts = [ 3030 ];

  etu.base.zfs.system.directories = [
    # Bind mount for persistent database for prometheus
    "/var/lib/prometheus2"
    # Bind mount for persistent database for grafana
    "/var/lib/grafana"
  ];
}
