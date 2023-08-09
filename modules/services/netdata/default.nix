{
  config,
  lib,
  myData,
  ...
}: {
  options.etu.services.netdata = {
    enable = lib.mkEnableOption "Enable services netdata service";
  };

  config = lib.mkIf config.etu.services.netdata.enable {
    # Bind mount for persistent data for jellyfin
    etu.base.zfs.system.directories = [
      "/var/lib/netdata"
    ];

    # Include secret with token
    age.secrets.netdata-claim-token-file = myData.ageModules.netdata-claim-token-file;

    # Enable netdata
    services.netdata.enable = true;
    services.netdata.claimTokenFile = config.age.secrets.netdata-claim-token-file.path;
  };
}
