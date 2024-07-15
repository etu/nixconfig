{
  config,
  myData,
  ...
}: {
  age.secrets = {
    inherit (myData.ageModules) homepage-dashboard-environment;
  };

  services.homepage-dashboard = {
    enable = true;
    environmentFile = config.age.secrets.homepage-dashboard-environment.path;
    bookmarks = [
      {
        Media = [
          {
            Bazarr = [
              {
                icon = "bazarr.svg";
                href = "/bazarr";
                description = "Subtitles";
              }
            ];
          }
          {
            Lidarr = [
              {
                icon = "lidarr.svg";
                href = "/lidarr";
                description = "Music";
              }
            ];
          }
          {
            Radarr = [
              {
                icon = "radarr.svg";
                href = "/radarr";
                description = "Movies";
              }
            ];
          }
          {
            Sonarr = [
              {
                icon = "sonarr.svg";
                href = "/sonarr";
                description = "Series";
              }
            ];
          }
          {
            NZBGet = [
              {
                icon = "sabnzbd.svg";
                href = "/nzbget";
                description = "NZBGet";
              }
            ];
          }
        ];
      }
      {
        Printer = [
          {
            Klipper = [
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
        Media = [
          {
            Sonarr = {
              icon = "sonarr.svg";
              description = "Series";
              widget = {
                type = "sonarr";
                url = "http://server-main-elis/sonarr";
                key = "{{HOMEPAGE_VAR_SONARR_API_KEY}}";
              };
            };
          }
          {
            Radarr = {
              icon = "radarr.svg";
              description = "Movies";
              widget = {
                type = "radarr";
                url = "http://server-main-elis/radarr";
                key = "{{HOMEPAGE_VAR_RADARR_API_KEY}}";
              };
            };
          }
          {
            Lidarr = {
              icon = "lidarr.svg";
              description = "Music";
              widget = {
                type = "lidarr";
                url = "http://server-main-elis/lidarr";
                key = "{{HOMEPAGE_VAR_LIDARR_API_KEY}}";
              };
            };
          }
          {
            Bazarr = {
              icon = "bazarr.svg";
              description = "Subtitles";
              widget = {
                type = "bazarr";
                url = "http://server-main-elis/bazarr";
                key = "{{HOMEPAGE_VAR_BAZARR_API_KEY}}";
              };
            };
          }
          {
            NZBGet = {
              icon = "sabnzbd.svg";
              description = "NZBGet";
              widget = {
                type = "nzbget";
                url = "http://server-main-elis/nzbget";
              };
            };
          }
        ];
      }
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
