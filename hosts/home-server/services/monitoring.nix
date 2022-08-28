{ config, pkgs, ... }:

let
  # Import age secrets paths and metadata.
  ageModules = (import ../../../data.nix).ageModules;

  zfsExporterPkg = let
    version = "2.2.5";
  in pkgs.buildGoModule {
    pname = "zfs-exporter";
    inherit version;

    src = pkgs.fetchFromGitHub {
      owner = "pdf";
      repo = "zfs_exporter";
      rev = "v${version}";
      sha256 = "sha256-FY3P2wmNWyr7mImc1PJs1G2Ae8rZvDzq0kRZfiRTzyc=";
    };

    vendorSha256 = "sha256-jQiw3HlqWcsjdadDdovCsDMBB3rnWtacfbtzDb5rc9c=";
  };

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
      ExecStart = "${zfsExporterPkg}/bin/zfs_exporter --properties.dataset-filesystem=available,compressratio,logicalused,quota,referenced,used,usedbydataset,written";
      Restart = "always";
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
  ];


  # Enable grafana.
  services.grafana.enable = true;
  services.grafana.addr = "0.0.0.0";
  services.grafana.port = 3030;
  services.grafana.domain = "grafana.elis.nu";

  # Set an admin password.
  services.grafana.security.adminUser = "admin";
  services.grafana.security.adminPasswordFile = "/var/lib/grafana/admin-password";

  # Provision datasources for me.
  services.grafana.provision.enable = true;
  services.grafana.provision.datasources = [
    {
      isDefault = true;
      name = "Prometheus";
      type = "prometheus";
      url = "http://127.0.0.1:${toString config.services.prometheus.port}";
    }
  ];

  # Decrypt secret to expected location.
  age.secrets.grafana-admin-password = ageModules.grafana-admin-password // {
    path = config.services.grafana.security.adminPasswordFile;
  };

  networking.firewall.allowedTCPPorts = [ 3030 ];

  etu.base.zfs.system.directories = [
    # Bind mount for persistent database for prometheus
    "/var/lib/prometheus2"
    # Bind mount for persistent database for grafana
    "/var/lib/grafana"
  ];
}
