{ config, lib, ... }:

let
  preStart = ''
    echo "Checking for nameservers to appear in /etc/resolv.conf";
    while ! grep nameserver /etc/resolv.conf > /dev/null; do
      echo "Waiting for nameservers to appear in /etc/resolv.conf";
      sleep 1;
    done
  '';

in
{
  # Add a pre start check for network to be up for certain services.
  config.systemd.services."docker-home-assistant".preStart = preStart;
  config.systemd.services."docker-mqtt".preStart = preStart;
  config.systemd.services."docker-zwavejs2mqtt".preStart = preStart;
  config.systemd.services."container@freshrss".preStart = preStart;
  config.systemd.services."container@usenet".preStart = preStart;
}
