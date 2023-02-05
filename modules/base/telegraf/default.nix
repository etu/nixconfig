{
  config,
  lib,
  myData,
  pkgs,
  ...
}: {
  options.etu.base.telegraf.enable = lib.mkEnableOption "Enable base telegraf reporting settings";

  config = lib.mkIf config.etu.base.telegraf.enable {
    services.telegraf = {
      enable = true;
      extraConfig = {
        agent = {
          interval = "10s";
          round_interval = true;
          metric_batch_size = 1000;
          metric_buffer_limit = 10000;
          collection_jitter = "0s";
          flush_interval = "10s";
          flush_jitter = "0s";
          precision = "0s";
          omit_hostname = false;
        };
        outputs.influxdb_v2 = {
          urls = ["https://influx.elis.nu"];
          token = "$TOKEN";
          organization = "default";
          bucket = "default";
        };
        inputs = {
          cpu = {};
          disk = {};
          diskio = {};
          kernel = {};
          mem = {};
          net = {};
          netstat = {};
          processes = {};
          swap = {};
          system = {};
          systemd_units = {};

          # Collect ZFS metrics
          exec = {
            commands = ["${pkgs.zfs}/libexec/zfs/zpool_influxdb"];
            timeout = "5s";
            data_format = "influx";
          };
        };
      };
      environmentFiles = [config.age.secrets.telegraf-env.path];
    };

    age.secrets.telegraf-env = myData.ageModules.telegraf-env;
  };
}
