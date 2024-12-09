{config, ...}: {
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts.${config.networking.hostName}.locations = let
    onlyLan = ''
      allow 100.0.0.0/8;
      allow 127.0.0.1/24;
      allow ::1;
      deny all;
    '';
  in {
    "/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.homepage-dashboard.listenPort}";
      recommendedProxySettings = true;
      extraConfig = onlyLan;
    };
    "/bazarr" = {
      proxyPass = "http://127.0.0.1:6767/bazarr";
      extraConfig = onlyLan;
    };
    "/sonarr" = {
      proxyPass = "http://127.0.0.1:8989/sonarr";
      extraConfig = onlyLan;
    };
    "/radarr" = {
      proxyPass = "http://127.0.0.1:7878/radarr";
      extraConfig = onlyLan;
    };
    "/lidarr" = {
      proxyPass = "http://127.0.0.1:8686/lidarr";
      extraConfig = onlyLan;
    };
    "~ ^/nzbget($|./*)" = {
      proxyPass = "http://127.0.0.1:6789";
      extraConfig =
        onlyLan
        + ''
          rewrite /nzbget/(.*) /$1 break;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        '';
    };
  };

  # Enable usenet related services in a container
  containers.usenet = {
    autoStart = true;
    config = {lib, ...}: {
      # The NixOS release to be compatible with for stateful data such as databases.
      system.stateVersion = config.etu.stateVersion;

      # dotnet for sonarr seems to be very old
      nixpkgs.config.permittedInsecurePackages = [
        "aspnetcore-runtime-6.0.36"
        "aspnetcore-runtime-wrapped-6.0.36"
        "dotnet-sdk-6.0.428"
        "dotnet-sdk-wrapped-6.0.428"
      ];

      # Disable documentation to make the system smaller.
      documentation.enable = false;
      documentation.doc.enable = false;
      documentation.info.enable = false;
      documentation.man.enable = false;

      # nzbget needs unrar
      nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) ["unrar"];

      services.bazarr = {
        enable = true;
        user = "downloads";
        group = "downloads";
      };
      services.sonarr = {
        enable = true;
        user = "downloads";
        group = "downloads";
      };
      services.radarr = {
        enable = true;
        user = "downloads";
        group = "downloads";
      };
      services.lidarr = {
        enable = true;
        user = "downloads";
        group = "downloads";
      };
      services.nzbget = {
        enable = true;
        user = "downloads";
        group = "downloads";
      };

      users.users.downloads = {
        group = "downloads";
        uid = 947;
        isSystemUser = true;
      };
      users.groups.downloads.gid = 947;
    };

    bindMounts = {
      bazarr = {
        mountPoint = "/var/lib/bazarr";
        hostPath = "${config.etu.dataPrefix}/var/lib/bazarr";
        isReadOnly = false;
      };
      sonarr = {
        mountPoint = "/var/lib/sonarr/.config/NzbDrone";
        hostPath = "${config.etu.dataPrefix}/var/lib/sonarr";
        isReadOnly = false;
      };
      radarr = {
        mountPoint = "/var/lib/radarr/.config/Radarr";
        hostPath = "${config.etu.dataPrefix}/var/lib/radarr";
        isReadOnly = false;
      };
      lidarr = {
        mountPoint = "/var/lib/lidarr/.config/Lidarr";
        hostPath = "${config.etu.dataPrefix}/var/lib/lidarr";
        isReadOnly = false;
      };
      nzbget = {
        mountPoint = "/var/lib/nzbget";
        hostPath = "${config.etu.dataPrefix}/var/lib/nzbget";
        isReadOnly = false;
      };
      nzbget-dst = {
        mountPoint = "/var/lib/nzbget-dst";
        hostPath = "/var/lib/nzbget-dst";
        isReadOnly = false;
      };
      media = {
        mountPoint = "/media";
        hostPath = "/media/zstorage/files";
        isReadOnly = false;
      };
    };
  };
}
