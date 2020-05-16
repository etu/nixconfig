{ config, lib, ... }:

{
  config.systemd.targets.cryptraid = {
    wants = [
      "media-legacy.mount"
      "container@jellyfin.service"
      "container@usenet.service"
      "nfs-server.service"
      "container@freshrss.service"
      "podman-magic-mirror.service"
    ];
  };

  config.systemd.services."container@jellyfin" = {
    after = [ "media-legacy.mount" ];
  };

  config.systemd.services."container@usenet" = {
    after = [ "media-legacy.mount" ];
  };

  config.systemd.services.nfs-server = {
    wantedBy = [ "cryptraid.target" ];
    after = [ "media-legacy.mount" ];
  };
}
