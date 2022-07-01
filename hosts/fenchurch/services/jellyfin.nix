{ config, pkgs, lib, ... }:

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

  # Bind mount for persistent data for jellyfin
  fileSystems."/var/lib/jellyfin" = {
    device = "/persistent/var/lib/jellyfin";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Enable vaapi on OS-level
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  # Allow jellyfin user to access the graphics card
  users.users.${config.services.jellyfin.user}.extraGroups = [ "video" "render" ];

  # Override default hardening measure from NixOS
  systemd.services.jellyfin.serviceConfig.PrivateDevices = lib.mkForce false;
  systemd.services.jellyfin.serviceConfig.DeviceAllow = lib.mkForce [ "/dev/dri/renderD128" ];

  # Enable jellyfin itself
  services.jellyfin.enable = true;

  # Open NGiNX port
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
