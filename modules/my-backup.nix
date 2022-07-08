{ config, lib, ... }:

let
  cfg = config.my.backup;

in
{
  config = lib.mkIf cfg.enable {
    # Enable syncoid for syncing snapshots.
    services.syncoid = {
      enable = cfg.enableSyncoid;
      interval = "*-*-* *:15:00";
      commonArgs = [ "--no-sync-snap" ];
      sshKey = "/var/lib/syncoid/.ssh/id_ed25519";
    };

    # Bind mount syncoid ssh key
    fileSystems."/var/lib/syncoid/.ssh" = lib.mkIf cfg.enableSyncoid {
      device = "/persistent/home/syncoid/.ssh";
      options = [ "bind" "noauto" "x-systemd.automount" ];
      noCheck = true;
    };
  };
}
