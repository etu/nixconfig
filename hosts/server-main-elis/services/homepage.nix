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
    services = [
      {
        "Printer Status" = [
          {
            "Printer Status" = {
              icon = "klipper.svg";
              description = "Creality Ender 6";
              widget = {
                type = "moonraker";
                url = "http://server-main-elis";
              };
            };
          }
        ];
      }
      {
        "Printer Camera" = [
          {
            "Printer Camera" = {
              icon = "klipper.svg";
              description = "Creality Ender 6 Camera";
              widget = {
                type = "mjpeg";
                stream = "http://server-main-elis/klipper/webcam?action=stream";
              };
            };
          }
        ];
      }
    ];
  };
}
