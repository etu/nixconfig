{ config, lib, myData, ... }:

{
  options.etu.services.freshrss = {
    enable = lib.mkEnableOption "Enable services freshrss service";
    hostname = lib.mkOption {
      type = lib.types.str;
      default = "freshrss.elis.nu";
      description = "Hostname to expose freshrss on";
    };
  };

  config = lib.mkIf config.etu.services.freshrss.enable {
    # Make sure to have nginx enabled
    services.nginx.enable = true;
    services.nginx.virtualHosts.${config.etu.services.freshrss.hostname} = {
      forceSSL = true;
      enableACME = true;
    };

    age.secrets = {
      inherit (myData.ageModules) freshrss-password-etu;
    };

    # Set up freshrss.
    services.freshrss = {
      enable = true;
      baseUrl = "https://${config.etu.services.freshrss.hostname}";
      virtualHost = config.etu.services.freshrss.hostname;

      # Set up my user
      defaultUser = config.etu.user.username;
      passwordFile = config.age.secrets.freshrss-password-etu.path;
    };

    etu.base.zfs.system.directories = [
      # Bind mount for persistent data for freshrss
      "/var/lib/freshrss"
    ];
  };
}
