{ config, pkgs, ... }:

{
  # Run docker container with the magic mirror software
  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers.magic-mirror = {
    image = "bastilimbach/docker-magicmirror";
    ports = [ "9000:8080" ];
    volumes = [
      "/persistent/var/lib/magic_mirror/config:/opt/magic_mirror/config"
      "/persistent/var/lib/magic_mirror/modules:/opt/magic_mirror/modules"
    ];
  };

  # Open firewall port for Magic Mirror
  networking.firewall.allowedTCPPorts = [ 9000 ];
}
