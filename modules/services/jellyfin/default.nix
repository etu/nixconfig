{
  config,
  lib,
  pkgs,
  ...
}: let
  # Fetch a specific version of nixpkgs to pin the jellyfin version.
  jellyfinPkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/6ad93a05a3daf6d49a3fda36ac0b9d76d17e683e.tar.gz";
    sha256 = "1dsmb30v10v63sjn0y76150zw2vflc33mvhn3f82n54i8zx2hzkw";
  }) {system = "x86_64-linux";};
in {
  options.etu.services.jellyfin = {
    enable = lib.mkEnableOption "Enable services jellyfin service";
    hostname = lib.mkOption {
      type = lib.types.str;
      default = "jellyfin.elis.nu";
      description = "Hostname to expose jellyfin on";
    };
  };

  config = lib.mkIf config.etu.services.jellyfin.enable {
    # Make sure to have nginx enabled
    services.nginx.enable = true;
    services.nginx.clientMaxBodySize = "20m"; # Increase body size since we handle video.
    services.nginx.virtualHosts.${config.etu.services.jellyfin.hostname} = let
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

    # Bind mount for persistent data for jellyfin
    etu.base.zfs.system.directories = [
      "/var/lib/jellyfin"
    ];

    # Enable vaapi on OS-level
    nixpkgs.config.packageOverrides = pkgs: {
      vaapiIntel = pkgs.vaapiIntel.override {enableHybridCodec = true;};
    };
    hardware.opengl = {
      enable = true;
      extraPackages = [
        pkgs.intel-media-driver
        pkgs.vaapiIntel
        pkgs.vaapiVdpau
        pkgs.libvdpau-va-gl
      ];
    };

    # Allow jellyfin user to access the graphics card
    users.users.${config.services.jellyfin.user}.extraGroups = ["video" "render"];

    # Override default hardening measure from NixOS
    systemd.services.jellyfin.serviceConfig.PrivateDevices = lib.mkForce false;
    systemd.services.jellyfin.serviceConfig.DeviceAllow = lib.mkForce ["/dev/dri/renderD128"];

    # Enable jellyfin itself
    services.jellyfin.enable = true;

    # Override jellyfin-web used by jellyfin to use an older version
    # of jellyfin-web to work on my LG TV.
    services.jellyfin.package = jellyfinPkgs.jellyfin;

    # Open NGiNX port
    networking.firewall.allowedTCPPorts = [80 443];
  };
}
