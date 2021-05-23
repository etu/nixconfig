{ config, lib, ... }:

let
  cfg = config.my.backup;

in
{
  options.my.backup.enable = lib.mkEnableOption "Enables backup related thingys.";
  options.my.backup.enableSanoid = lib.mkEnableOption "Enables snapshot creation.";
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
        value = { settings = { frequently = 7; hourly = 36; daily = 14; weekly = 4; monthly = 0; }; };
      }) cfg.filesystems);
    };
  };
}
