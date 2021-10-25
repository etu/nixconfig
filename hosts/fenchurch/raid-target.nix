{ config, lib, ... }:

let
  preStart = ''
    echo "Checking for nameservers to appear in /etc/resolv.conf";
    while ! grep nameserver /etc/resolv.conf > /dev/null; do
      echo "Waiting for nameservers to appear in /etc/resolv.conf";
      sleep 1;
    done
  '';
in {
  config.systemd.targets.cryptraid = {
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [
      # Containers
      "container@freshrss.service"
      "container@jellyfin.service"
      "container@usenet.service"
      "podman-home-assistant.service"
      "podman-mqtt.service"

      "media-legacy.mount"
      "nfs-server.service"
      "systemd-cryptsetup@cryptraid.service"
    ];
  };

  config.systemd.services."container@jellyfin".after = [ "media-legacy.mount" ];
  config.systemd.services."container@usenet".after = [ "media-legacy.mount" ];

  config.systemd.services.nfs-server = {
    wantedBy = lib.mkForce [ "cryptraid.target" ];
    after = [ "media-legacy.mount" ];
  };

  # Add a pre start check for network to be up for certain services.
  config.systemd.services."podman-home-assistant".preStart = preStart;
  config.systemd.services."podman-mqtt".preStart = preStart;
  config.systemd.services."podman-zwavejs2mqtt".preStart = preStart;
  config.systemd.services."container@freshrss".preStart = preStart;
  config.systemd.services."container@jellyfin".preStart = preStart;
  config.systemd.services."container@usenet".preStart = preStart;
}
