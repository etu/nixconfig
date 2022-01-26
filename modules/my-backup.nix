{ config, lib, ... }:

let
  cfg = config.my.backup;

  # Import my ssh public keys
  keys = import ../data/pubkeys.nix;
in
{
  config = lib.mkIf cfg.enable {
    # Enable sanoid snapshoting with rules for creating snapshots.
    services.sanoid = {
      enable = cfg.enableSanoid;
      interval = "*-*-* *:00,15,30,45:00";

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

      # Persistent snapshotting rules
      templates.persistent = {
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

    # Add known hosts so syncoid don't bail out on unknown hosts
    programs.ssh.knownHosts = {
      fenchurch-ec = {
        extraHostNames = [ "home.elis.nu" "local.elis.nu" "192.168.0.101" ];
        publicKey = keys.systems.fenchurch.ec;
      };
      fenchurch-rsa = {
        extraHostNames = [ "home.elis.nu" "local.elis.nu" "192.168.1.101" ];
        publicKey = keys.systems.fenchurch.rsa;
      };
      vps04-ec = {
        extraHostNames = [ "vps04.elis.nu" ];
        publicKey = keys.systems.vps04.ec;
      };
      vps04-rsa = {
        extraHostNames = [ "vps04.elis.nu" ];
        publicKey = keys.systems.vps04.rsa;
      };
      vps05-ec = {
        extraHostNames = [ "vps05.elis.nu" ];
        publicKey = keys.systems.vps05.ec;
      };
      vps05-rsa = {
        extraHostNames = [ "vps05.elis.nu" ];
        publicKey = keys.systems.vps05.rsa;
      };
      kodi-ec = {
        extraHostNames = [ "192.168.0.105" ];
        publicKey = keys.systems.kodi.ec;
      };
      kodi-rsa = {
        extraHostNames = [ "192.168.0.105" ];
        publicKey = keys.systems.kodi.rsa;
      };
    };
  };
}
