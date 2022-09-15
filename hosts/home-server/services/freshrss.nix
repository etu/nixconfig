{ config, pkgs, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "freshrss.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:18888/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };
  };

  containers.freshrss =
    let
      dataDir = "/var/lib/freshrss";

      # Get FreshRSS source
      freshrssSrc = pkgs.fetchFromGitHub {
        owner = "FreshRSS";
        repo = "FreshRSS";
        rev = "1.20.0";
        sha256 = "sha256-mzhEw2kFv77SmeuEx66n9ujWMscZO3+03tHimk1/vk4=";
      };
    in
    {
      autoStart = true;
      bindMounts = {
        "freshrss-data" = {
          mountPoint = "${dataDir}/data";
          hostPath = "${config.etu.dataPrefix}/var/lib/freshrss/data";
          isReadOnly = false;
        };
        "postgres-data" = {
          mountPoint = "/var/lib/postgresql/11";
          hostPath = "${config.etu.dataPrefix}/var/lib/freshrss/db";
          isReadOnly = false;
        };
      };

      config = { config, pkgs, ... }: {
        # The NixOS release to be compatible with for stateful data such as databases.
        system.stateVersion = "22.05";

        # Disable documentation to make the system smaller.
        documentation.enable = false;
        documentation.doc.enable = false;
        documentation.info.enable = false;
        documentation.man.enable = false;

        # Set up NGiNX
        services.nginx.enable = true;

        # Set up host in NGiNX
        services.nginx.virtualHosts."freshrss.elis.nu" = {
          root = "${dataDir}/p";
          listen = [{ addr = "0.0.0.0"; port = 18888; }];

          locations."~ \.php$".extraConfig = ''
            fastcgi_pass unix:${config.services.phpfpm.pools.freshrss.socket};
            fastcgi_index index.php;
            include ${pkgs.nginx}/conf/fastcgi_params;
            include ${pkgs.nginx}/conf/fastcgi.conf;
          '';

          locations."/" = {
            tryFiles = "$uri $uri/ index.php";
            index = "index.php index.html index.htm";
          };
        };

        # Set up PHP Pool
        services.phpfpm.pools.freshrss = {
          user = "nginx";
          settings = {
            "listen.group" = "nginx";
            "listen.mode" = "0600";
            "listen.owner" = "nginx";
            "pm" = "dynamic";
            "pm.max_children" = 5;
            "pm.max_requests" = 500;
            "pm.max_spare_servers" = 3;
            "pm.min_spare_servers" = 1;
            "pm.start_servers" = 2;
          };
        };

        # Set up postgresql
        services.postgresql = {
          enable = true;
          package = pkgs.postgresql_11;
          dataDir = "/var/lib/postgresql/11";
        };

        # Set up FreshRSS service to configure the web root
        systemd.services.freshrss-config = {
          serviceConfig.Type = "oneshot";
          script = ''
            mkdir -m 755 -p ${dataDir}
            cd ${dataDir}

            # Delete all but the "data" folder
            ls | grep -v data | while read line; do rm -rf $line; done || true

            # Copy all needed source files
            cp -vr $(find ${freshrssSrc} -maxdepth 1 -mindepth 1 | grep -e '/cli' -e '/config' -e '/index' -e '/constants.php' -e '/app' -e '/extensions' -e '/p' -e '/lib') ${dataDir}

            # Copy the user data template directory
            cp -vr ${freshrssSrc}/data ${dataDir}

            # Remove do-install.txt if we're already set up
            if test -e ${dataDir}/data/config.php; then
              rm ${dataDir}/data/do-install.txt
            fi

            # Set permissions on user data
            chmod -R u=rwX,g=rwX,o=-rwx ${dataDir}/data
            chown -R nginx:nginx ${dataDir}/data
          '';
          wantedBy = [ "multi-user.target" ];
        };

        # Updater to run import of feeds.
        systemd.services.freshrss-updater = {
          description = "freshrss feed updater";
          after = [ "freshrss-config.service" ];
          wantedBy = [ "multi-user.target" ];
          startAt = "*:0/5";

          serviceConfig = {
            Type = "oneshot";
            User = "nginx";
            Group = "nginx";
            WorkingDirectory = dataDir;
            ExecStart = "${pkgs.php}/bin/php ${dataDir}/app/actualize_script.php";
          };
        };
      };
    };
}
