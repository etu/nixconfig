{ config, lib, ... }:

{
  options.etu.base.sanoid = {
    enable = lib.mkEnableOption "Enable base sanoid settings";
    datasets = lib.mkOption {
      default = { };
      description = "services.sanoid.datasets to snapshot";
    };
  };

  config = lib.mkIf config.etu.base.sanoid.enable {
    # Enable sanoid snapshoting with rules for creating snapshots.
    services.sanoid = {
      enable = true;
      interval = "*-*-* *:00,15,30,45:00";

      datasets = config.etu.base.sanoid.datasets;

      # Home snapshotting rules
      templates.home = {
        autosnap = true;
        autoprune = true;
        frequently = 3;
        hourly = 23;
        daily = 6;
        weekly = 3;
        monthly = 2;
      };

      # Data snapshotting rules
      templates.data = {
        autosnap = true;
        autoprune = true;
        frequently = 0;
        hourly = 0;
        daily = 6;
        weekly = 3;
        monthly = 2;
      };

      # Bulk storage snapshotting rules
      templates.storage = {
        autosnap = true;
        autoprune = true;
        frequently = 0;
        hourly = 0;
        daily = 6;
        weekly = 3;
        monthly = 2;
      };
    };
  };
}
