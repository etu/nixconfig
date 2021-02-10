{ config, pkgs, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts =
    let
      onlyLan = ''
        allow 10.3.0.0/24;
        deny all;
      '';
    in
    {
      # Bazarr
      "subs.lan".locations."/" = {
        proxyPass = "http://127.0.0.1:6767/";
        extraConfig = onlyLan;
      };
      # Sonarr
      "series.lan".locations."/" = {
        proxyPass = "http://127.0.0.1:8989/";
        extraConfig = onlyLan;
      };
      # Radarr
      "movies.lan".locations."/" = {
        proxyPass = "http://127.0.0.1:7878/";
        extraConfig = onlyLan;
      };
      # Lidarr
      "music.lan".locations."/" = {
        proxyPass = "http://127.0.0.1:8686/";
        extraConfig = onlyLan;
      };
      # Nzbget
      "nzbget.lan".locations."/" = {
        proxyPass = "http://127.0.0.1:6789/";
        extraConfig = onlyLan;
      };
    };

  # nzbget needs unrar and p7zip
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [ pkgs.p7zip.name ];

  # Enable usenet related services in a container
  containers.usenet = {
    config = { config, pkgs, ... }: {
      nixpkgs.config.allowUnfree = true;

      services.bazarr = { enable = true; user = "downloads"; group = "downloads"; };
      services.sonarr = { enable = true; user = "downloads"; group = "downloads"; };
      services.radarr = { enable = true; user = "downloads"; group = "downloads"; };
      services.lidarr = { enable = true; user = "downloads"; group = "downloads"; };
      services.nzbget = { enable = true; user = "downloads"; group = "downloads"; };

      users.users.downloads = { group = "downloads"; uid = 947; };
      users.groups.downloads.gid = 947;
    };

    forwardPorts = [
      { containerPort = 6767; hostPort = 6767; protocol = "tcp"; } # Bazarr
      { containerPort = 8989; hostPort = 8989; protocol = "tcp"; } # Sonarr
      { containerPort = 7878; hostPort = 7878; protocol = "tcp"; } # Radarr
      { containerPort = 8686; hostPort = 8686; protocol = "tcp"; } # Lidarr
      { containerPort = 6789; hostPort = 6789; protocol = "tcp"; } # NzbGet
    ];

    bindMounts = {
      "bazarr" = {
        mountPoint = "/var/lib/bazarr";
        hostPath = "/persistent/var/lib/bazarr";
        isReadOnly = false;
      };
      "sonarr" = {
        mountPoint = "/var/lib/sonarr/.config/NzbDrone";
        hostPath = "/persistent/var/lib/sonarr";
        isReadOnly = false;
      };
      "radarr" = {
        mountPoint = "/var/lib/radarr/.config/Radarr";
        hostPath = "/persistent/var/lib/radarr";
        isReadOnly = false;
      };
      "lidarr" = {
        mountPoint = "/var/lib/lidarr/.config/Lidarr";
        hostPath = "/persistent/var/lib/lidarr";
        isReadOnly = false;
      };
      "nzbget" = {
        mountPoint = "/var/lib/nzbget";
        hostPath = "/persistent/var/lib/nzbget";
        isReadOnly = false;
      };
      "nzbget-dst" = {
        mountPoint = "/var/lib/nzbget-dst";
        hostPath = "/var/lib/nzbget-dst";
        isReadOnly = false;
      };
      "hactar-media" = {
        mountPoint = "/media";
        hostPath = "/media/legacy/files";
        isReadOnly = false;
      };
    };
  };
}
