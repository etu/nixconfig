{ config, lib, ... }:

{
  config.systemd.targets.cryptraid = {
    wants = [
      "systemd-cryptsetup@cryptraid.service"
      "media-legacy.mount"
      "container@jellyfin.service"
      "container@usenet.service"
      "nfs-server.service"
      "container@freshrss.service"
      "podman-magic-mirror.service"
      "container@matomo.service"
    ];
  };

  config.systemd.services."container@jellyfin".after = [ "media-legacy.mount" ];
  config.systemd.services."container@usenet".after = [ "media-legacy.mount" ];

  config.systemd.services.nfs-server = {
    wantedBy = lib.mkForce [ "cryptraid.target" ];
    after = [ "media-legacy.mount" ];
  };
}
