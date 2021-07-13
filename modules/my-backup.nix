{ config, lib, ... }:

let
  cfg = config.my.backup;

in
{
  options.my.backup.enable = lib.mkEnableOption "Enables backup related thingys.";
  options.my.backup.enableSanoid = lib.mkEnableOption "Enables snapshot creation.";
  options.my.backup.enableSyncoid = lib.mkEnableOption "Enables snapshot syncing.";

  config = lib.mkIf cfg.enable {
    # Enable sanoid snapshoting with rules for creating snapshots.
    services.sanoid = {
      enable = cfg.enableSanoid;
      interval = "*-*-* *:00,15,30,45:00";

      templates.default = {
        autosnap = true;
        autoprune = true;
        frequently = 7;
        hourly = 36;
        daily = 14;
        weekly = 4;
        monthly = 2;
      };
    };

    # Enable syncoid for syncing snapshots.
    services.syncoid = {
      enable = cfg.enableSyncoid;
      interval = "*-*-* *:15:00";
      commonArgs = [ "--no-sync-snap" ];
      sshKey = "/root/.ssh/id_ed25519";
      user = "root";
      group = "root";
    };
  };
}
