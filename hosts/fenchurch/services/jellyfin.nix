{ config, pkgs, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "jellyfin.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyWebsockets = true;
      locations."/".proxyPass = "http://127.0.0.1:8096/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };
  };

  # Enable Jellyfin
  containers.jellyfin = {
    autoStart = true;
    additionalCapabilities = [ "CAP_IPC_LOCK" ];
    config = { config, pkgs, ... }: {
      # The NixOS release to be compatible with for stateful data such as databases.
      system.stateVersion = "22.05";

      # Disable documentation to make the system smaller.
      documentation.enable = false;
      documentation.doc.enable = false;
      documentation.info.enable = false;
      documentation.man.enable = false;

      services.jellyfin = { enable = true; user = "downloads"; group = "downloads"; };

      users.users.downloads = { group = "downloads"; uid = 947; isSystemUser = true; };
      users.groups.downloads.gid = 947;
    };
    forwardPorts = [
      { containerPort = 8096; hostPort = 8096; protocol = "tcp"; }
    ];
    bindMounts = {
      "jellyfin" = {
        mountPoint = "/var/lib/jellyfin";
        hostPath = "/persistent/var/lib/jellyfin";
        isReadOnly = false;
      };
      "media" = {
        mountPoint = "/media";
        hostPath = "/media/zstorage/files";
        isReadOnly = true;
      };
    };
  };

  # Open NGiNX port
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
