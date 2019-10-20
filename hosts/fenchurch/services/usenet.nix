{ config, pkgs, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "series.elis.nu".locations."/".proxyPass = "http://127.0.0.1:8989/"; # Sonarr
    "movies.elis.nu".locations."/".proxyPass = "http://127.0.0.1:7878/"; # Radarr
    "music.elis.nu".locations."/".proxyPass  = "http://127.0.0.1:8686/"; # Lidarr
    "nzbget.elis.nu".locations."/".proxyPass = "http://127.0.0.1:6789/"; # Nzbget
  };

  # Enable usenet related services in a container
  containers.usenet = {
    autoStart = true;
    config = { config, pkgs, ... }: {
      nixpkgs.config.allowUnfree = true; # nzbget needs unrar

      services.sonarr = { enable = true; user = "downloads"; group = "downloads"; };
      services.radarr = { enable = true; user = "downloads"; group = "downloads"; };
      services.lidarr = { enable = true; user = "downloads"; group = "downloads"; };
      services.nzbget = { enable = true; user = "downloads"; group = "downloads"; };

      users.users.downloads = { group = "downloads"; uid = 947; };
      users.groups.downloads.gid = 947;

      # Don't know why I need this, but dns resolvs didn't work without it
      environment.systemPackages = [ pkgs.bind ];
    };

    forwardPorts = [
      { containerPort = 8989; hostPort = 8989; protocol = "tcp"; } # Sonarr
      { containerPort = 7878; hostPort = 7878; protocol = "tcp"; } # Radarr
      { containerPort = 8686; hostPort = 8686; protocol = "tcp"; } # Lidarr
      { containerPort = 6789; hostPort = 6789; protocol = "tcp"; } # NzbGet
    ];

    bindMounts = {
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
        mountPoint = "/var/empty/.config/Lidarr";
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
        hostPath = "/mnt/hactar";
        isReadOnly = false;
      };
    };
  };
}
