{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.base.syncoid = {
    enable = lib.mkEnableOption "Enable base syncoid settings";
    commands = lib.mkOption {
      default = { };
      description = "services.syncoid.commands to sync";
    };
    source.enable = lib.mkEnableOption "Install compression/buffering tools syncoid uses when pulling from this host";
  };

  config = lib.mkMerge [
    (lib.mkIf config.etu.base.syncoid.enable {
      # Enable syncoid for syncing snapshots.
      services.syncoid = {
        enable = true;
        interval = "*-*-* *:15:00";
        commonArgs = [ "--no-sync-snap" ];
        sshKey = "/var/lib/syncoid/.ssh/id_ed25519";
        inherit (config.etu.base.syncoid) commands;
      };
    })

    (lib.mkIf config.etu.base.syncoid.source.enable {
      # syncoid shells out to these on the source side over SSH; without
      # them it silently falls back to sending uncompressed, unbuffered.
      environment.systemPackages = [
        pkgs.lzop
        pkgs.mbuffer
      ];
    })
  ];
}
