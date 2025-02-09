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
        Bookmarks = [
          {
            Beszel = [
              {
                icon = "beszel.svg";
                href = "http://server-main-elis:6432";
                description = "Beszel";
              }
            ];
          }
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
    ];
    services = [
      {
        Calendar = [
          {
            Calendar = {
              icon = "google-calendar.svg";
              description = "Media Calendar";
              widget = {
                type = "calendar";
                view = "monthly";
                showTime = "true";
                timezone = "Europe/Stockholm";
                integrations = [
                  {
                    type = "sonarr";
                    service_group = "Media";
                    service_name = "Sonarr";
                  }
                  {
                    type = "radarr";
                    service_group = "Media";
                    service_name = "Radarr";
                  }
                ];
              };
            };
          }
        ];
      }
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
    ];
  };
}
