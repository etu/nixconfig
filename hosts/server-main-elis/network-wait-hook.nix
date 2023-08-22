{config, ...}: let
  preStart = ''
    echo "Checking for nameservers to appear in /etc/resolv.conf";
    while ! grep nameserver /etc/resolv.conf > /dev/null; do
      echo "Waiting for nameservers to appear in /etc/resolv.conf";
      sleep 1;
    done
  '';
in {
  # Add a pre start check for network to be up for certain services.
  config.systemd.services."cloudflare-dyndns".preStart = preStart;
  config.systemd.services."container@usenet".preStart = preStart;
  config.systemd.services."freshrss-config".preStart = preStart;
  config.systemd.services."freshrss-updater".preStart = preStart;
  config.systemd.services."podman-home-assistant".preStart = preStart;
  config.systemd.services."podman-mqtt".preStart = preStart;
  config.systemd.services."podman-nzbget-exporter".preStart = preStart;
  config.systemd.services."podman-zwavejs2mqtt".preStart = preStart;
}
