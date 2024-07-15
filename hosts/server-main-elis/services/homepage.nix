_: {
  services.homepage-dashboard = {
    enable = true;
    bookmarks = [
      {
        Media = [
          {
            "Bazarr" = [
              {
                icon = "bazarr.svg";
                href = "/bazarr";
                description = "Subtitles";
              }
            ];
          }
          {
            "Lidarr" = [
              {
                icon = "lidarr.svg";
                href = "/lidarr";
                description = "Music";
              }
            ];
          }
          {
            "Radarr" = [
              {
                icon = "radarr.svg";
                href = "/radarr";
                description = "Movies";
              }
            ];
          }
          {
            "Sonarr" = [
              {
                icon = "sonarr.svg";
                href = "/sonarr";
                description = "Series";
              }
            ];
          }
          {
            "Nzbget" = [
              {
                icon = "sabnzbd.svg";
                href = "/nzbget";
                description = "Nzbget";
              }
            ];
          }
        ];
      }
      {
        Printer = [
          {
            "Klipper" = [
              {
                icon = "klipper.svg";
                href = "/klipper";
                description = "Creality Ender 6";
              }
            ];
          }
        ];
      }
    ];
  };
}
