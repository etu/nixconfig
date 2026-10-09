{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.etu.services.manyfold;
  dataDir = "${config.etu.dataPrefix}/var/lib/manyfold";
  secretKeyBaseFile = "${dataDir}/secret_key_base.env";
in
{
  options.etu.services.manyfold = {
    enable = lib.mkEnableOption "Enable services manyfold service";
    hostname = lib.mkOption {
      type = lib.types.str;
      default = "manyfold.elis.nu";
      description = "Hostname to expose manyfold on";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 3214;
      description = "Port the manyfold web container listens to on the host";
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman.autoPrune.enable = true;
    virtualisation.podman.autoPrune.flags = [ "--all" ];

    # Make sure to have nginx enabled, reverse proxying to the web container.
    services.nginx.enable = true;
    services.nginx.virtualHosts.${cfg.hostname}.locations."/".proxyPass = "http://127.0.0.1:${toString cfg.port}";

    # Generate a persistent SECRET_KEY_BASE on first start, since manyfold
    # (a Rails app) requires one to be set in production.
    systemd.services.manyfold-secret-key-base = {
      description = "Generate manyfold secret key base";
      before = [
        "podman-manyfold.service"
        "podman-manyfold-worker.service"
      ];
      wantedBy = [
        "podman-manyfold.service"
        "podman-manyfold-worker.service"
      ];
      serviceConfig.Type = "oneshot";
      script = ''
        if [ ! -f ${secretKeyBaseFile} ]; then
          install -d -m 0700 ${dataDir}
          printf 'SECRET_KEY_BASE=%s\n' "$(${pkgs.openssl}/bin/openssl rand -hex 64)" > ${secretKeyBaseFile}
        fi
      '';
    };

    virtualisation.oci-containers.containers = {
      manyfold-redis = {
        image = "docker.io/library/redis:7";
        volumes = [
          "${dataDir}/redis:/data"
        ];
        extraOptions = [ "--network=host" ];
        cmd = [
          "redis-server"
          "--port"
          "16379"
        ];
      };

      manyfold = {
        image = "ghcr.io/manyfold3d/manyfold:latest";
        environment = {
          RAILS_ENV = "production";
          REDIS_URL = "redis://127.0.0.1:16379/0";
          DATABASE_ADAPTER = "sqlite3";
          PORT = toString cfg.port;
        };
        environmentFiles = [ secretKeyBaseFile ];
        volumes = [
          "${dataDir}/storage:/app/storage"
          "${dataDir}/library:/library"
        ];
        extraOptions = [ "--network=host" ];
        dependsOn = [ "manyfold-redis" ];
      };

      manyfold-worker = {
        image = "ghcr.io/manyfold3d/manyfold:latest";
        cmd = [
          "bundle"
          "exec"
          "sidekiq"
        ];
        environment = {
          RAILS_ENV = "production";
          REDIS_URL = "redis://127.0.0.1:16379/0";
          DATABASE_ADAPTER = "sqlite3";
        };
        environmentFiles = [ secretKeyBaseFile ];
        volumes = [
          "${dataDir}/storage:/app/storage"
          "${dataDir}/library:/library"
        ];
        extraOptions = [ "--network=host" ];
        dependsOn = [ "manyfold-redis" ];
      };
    };

    # Bind mount for persistent data for manyfold
    etu.base.zfs.system.directories = [
      "/var/lib/manyfold"
    ];
  };
}
