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

  # Persistent directory mounts
  etu.base.zfs.system.directories = [
    "/var/lib/bazarr"
    "/var/lib/lidarr"
    "/var/lib/nzbget"
    "/var/lib/radarr"
    "/var/lib/sonarr"
  ];

  # nzbget needs unrar
  etu.base.nix.allowUnfree = ["unrar"];

  # dotnet for sonarr seems to be very old
  nixpkgs.config.permittedInsecurePackages = [
    "aspnetcore-runtime-6.0.36"
    "aspnetcore-runtime-wrapped-6.0.36"
    "dotnet-sdk-6.0.428"
    "dotnet-sdk-wrapped-6.0.428"
  ];

  # Enable services
  services.bazarr = {
    enable = true;
    user = "downloads";
    group = "downloads";
  };
  services.nzbget = {
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
}
