{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.services.netdata = {
    enable = lib.mkEnableOption "Enable services netdata service";
  };

  config = lib.mkIf config.etu.services.netdata.enable {
    etu.base.zfs.local.directories = [
      "/var/cache/netdata"
    ];

    # Bind mount for persistent data for netdata
    etu.base.zfs.system.directories = [
      "/var/lib/netdata"
    ];

    # Allow to install netdata
    etu.base.nix.allowUnfree = [
      "netdata"
    ];

    # Include secret with token
    age.secrets.netdata-claim-token-file = config.etu.data.ageModules.netdata-claim-token-file;

    # Enable netdata
    services.netdata.enable = true;
    services.netdata.package = pkgs.netdataCloud;
    services.netdata.claimTokenFile = config.age.secrets.netdata-claim-token-file.path;
  };
}
