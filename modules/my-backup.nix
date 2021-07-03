{ config, lib, ... }:

let
  cfg = config.my.backup;

in
{
  options.my.backup.enable = lib.mkEnableOption "Enables backup related thingys.";
  options.my.backup.enableSanoid = lib.mkEnableOption "Enables snapshot creation.";
  options.my.backup.enableSyncoid = lib.mkEnableOption "Enables snapshot syncing.";
  options.my.backup.filesystems = lib.mkOption {
    default = [];
    example = [ "zroot/home" "zroot/persistent" ];
    description = "A list of strings of filesystems to snapshot and back up.";
    type = lib.types.listOf lib.types.str;
  };

  config = lib.mkIf cfg.enable {
    # Enable sanoid snapshoting with rules for creating snapshots.
    services.sanoid = {
      enable = cfg.enableSanoid;
      interval = "*-*-* *:00,15,30,45:00";

      #
      # Make the datasets configuration for sanoid that normally looks
      # like:
      #
      # datasets."zroot/persistent".settings = { daily = 14; ... };
      # datasets."zroot/home".settings = { daily = 14; ... };
      #
      datasets = builtins.listToAttrs (map (datasetName: {
        name = datasetName;
        value = {
          frequently = 7;
          hourly = 36;
          daily = 14;
          weekly = 4;
        };
      }) cfg.filesystems);
    };

    # Enable syncoid for syncing snapshots.
    services.syncoid = {
      enable = cfg.enableSyncoid;
      interval = "*-*-* *:15:00";
      commonArgs = [ "--no-sync-snap" ];
      sshKey = "/root/.ssh/id_ed25519";
      user = "root";
      group = "root";

      #
      # Make the commands configuration for syncoid that normally looks
      # like:
      #
      # commands."zroot/persistent".target = "user@host:path/to/dataset";
      # commands."zroot/home".target = "user@host:path/to/dataset";
      #
      commands = builtins.listToAttrs (map (datasetName: {
        name = datasetName;
        value = { target = "root@home.elis.nu:zroot/backups/${config.networking.hostName}/${datasetName}"; };
      }) cfg.filesystems);
    };
  };
}
