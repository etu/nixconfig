{ config, ... }:

{
  # Enable murmur
  services.murmur.enable = true;
  services.murmur.registerName = "Elis mumble";
  services.murmur.welcometext = "Välkommen till min mumbleserver.";

  # Firewall rules
  networking.firewall.allowedTCPPorts = [ 64738 ];
  networking.firewall.allowedUDPPorts = [ 64738 ];

  # Bind mount for persistent database for murmur
  fileSystems."/var/lib/murmur" = {
    device = "/persistent/var/lib/murmur";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };
}
