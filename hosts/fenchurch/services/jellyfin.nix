{ config, pkgs, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "jellyfin.elis.nu".locations."/".proxyPass = "http://127.0.0.1:8096/";
  };

  # Enable Jellyfin
  containers.jellyfin = {
    autoStart = true;
    additionalCapabilities = [ "CAP_IPC_LOCK" ];
    config = { config, pkgs, ... }: {
      services.jellyfin = { enable = true; user = "downloads"; group = "downloads"; };

      users.users.downloads = { group = "downloads"; uid = 947; };
      users.groups.downloads.gid = 947;
    };
    forwardPorts = [
      { containerPort = 8096; hostPort = 8096; protocol = "tcp"; }
    ];
    bindMounts = {
      "/var/lib/jellyfin" = {
        mountPoint = "/var/lib/jellyfin";
        hostPath = "/persistent/var/lib/jellyfin";
        isReadOnly = false;
      };
      "/mnt/hactar" = {
        mountPoint = "/media";
        hostPath = "/media/legacy/files";
        isReadOnly = true;
      };
    };
  };

  # Open NGiNX port
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
