{ config, pkgs, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.clientMaxBodySize = "20m"; # Increase body size since we handle video.
  services.nginx.virtualHosts = {
    "jellyfin.elis.nu" = let
      locationConfig = {
        proxyWebsockets = true;
        proxyPass = "http://127.0.0.1:8096/";
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
          proxy_set_header X-Forwarded-Protocol $scheme;
          proxy_set_header X-Forwarded-Host $http_host;

          # Disable buffering when the nginx proxy gets very resource heavy upon streaming
          proxy_buffering off;
      '';
      };
    in {
      forceSSL = true;
      enableACME = true;
      locations."/" = locationConfig;
      locations."= /web/" = locationConfig;
      locations."/socket" = locationConfig;
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

      # Pull in patch for jellyfin-web that enables transcode of DTS
      # audio for modern LG TV's that doesn't support DTS (any
      # more). This patch is part of the next 10.8.0 release.
      nixpkgs.overlays = [
        (self: super: {
          jellyfin-web = super.jellyfin-web.overrideAttrs (oa: {
            patches = [
              (pkgs.fetchpatch {
                name = "2971-webos-dts-support.patch";
                url = "https://github.com/jellyfin/jellyfin-web/pull/2971.patch";
                sha256 = "sha256-9P8qMm+IgV2W10ImF9UTzUy6sroxKqATG5MiroQSgY4=";
              })
            ];
          });
        })
      ];

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
