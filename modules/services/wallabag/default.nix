{
  pkgs,
  config,
  lib,
  myData,
  ...
}: let
  datadir = "/var/lib/wallabag";

  configFile = pkgs.writeTextFile {
    name = "wallabag-config";
    text = builtins.toJSON {
      # Based on https://github.com/wallabag/wallabag/blob/c018d41f908343cb79bfc09f4ed5955c46f65b15/app/config/parameters.yml.dist
      parameters = {
        database_driver = "pdo_sqlite";
        database_driver_class = "~";
        database_host = "127.0.0.1";
        database_port = "~";
        database_name = "wallabag";
        database_user = "root";
        database_password = "~";
        database_table_prefix = "wallabag_";
        database_socket = "~";
        database_path = "/var/lib/wallabag/data/db/wallabag.sqlite";
        database_charset = "utf8";

        domain_name = "https://${config.etu.services.wallabag.hostname}";
        server_name = "Wallabag";

        mailer_transport = null;
        mailer_user = null;
        mailer_password = null;
        mailer_host = null;
        mailer_port = null;
        mailer_encryption = null;
        mailer_auth_mode = null;

        locale = "en";

        # A secret key that's used to generate certain security-related tokens
        # We use agenix so we need to substitute it at activation time.
        secret = "@secret@";

        # two factor stuff
        twofactor_auth = true;
        twofactor_sender = "no-reply@wallabag.org";

        # fosuser stuff
        fosuser_registration = false;
        fosuser_confirmation = false;

        # how long the access token should live in seconds for the API
        fos_oauth_server_access_token_lifetime = 3600;
        # how long the refresh token should life in seconds for the API
        fos_oauth_server_refresh_token_lifetime = 1209600;

        from_email = "no-reply@wallabag.org";

        # rss_limit = 50;

        # RabbitMQ processing
        rabbitmq_host = null;
        rabbitmq_port = null;
        rabbitmq_user = null;
        rabbitmq_password = null;
        rabbitmq_prefetch_count = null;

        # Redis processing
        redis_scheme = null;
        redis_host = null;
        redis_port = null;
        redis_path = null;
        redis_password = null;

        # sentry logging
        sentry_dsn = null;
      };
    };
  };

  appDir = pkgs.buildEnv {
    name = "wallabag-app-dir";
    ignoreCollisions = true;
    checkCollisionContents = false;
    paths = [
      (pkgs.runCommandLocal "wallabag-config-link" {} ''
        mkdir -p "$out/config"
        ln -s "/etc/wallabag/parameters.yml" "$out/config/parameters.yml"
      '')
      "${pkgs.wallabag}/app"
    ];
  };
in {
  options.etu.services.wallabag = {
    enable = lib.mkEnableOption "Enable services wallabag service";
    hostname = lib.mkOption {
      type = lib.types.str;
      default = "wallabag.elis.nu";
      description = "Hostname to expose wallabag on";
    };
  };

  config = lib.mkIf config.etu.services.wallabag.enable {
    # Create user and group
    users.groups.wallabag.gid = 912;
    users.users.wallabag = {
      isSystemUser = true;
      group = "wallabag";
      description = "Wallabag daemon user";
      home = datadir;
      uid = 912;
    };

    # Create database and make sure the user exists
    services.postgresql = {
      ensureDatabases = ["wallabag"];
      ensureUsers = [
        {
          name = "wallabag";
          ensurePermissions."DATABASE wallabag" = "ALL PRIVILEGES";
        }
      ];
    };

    # Set up nginx
    services.nginx.enable = true;
    services.nginx.virtualHosts.${config.etu.services.wallabag.hostname} = {
      forceSSL = true;
      enableACME = true;
      root = "${pkgs.wallabag}/web";
      extraConfig = ''
        add_header X-Frame-Options SAMEORIGIN;
        add_header X-Content-Type-Options nosniff;
        add_header X-XSS-Protection "1; mode=block";
      '';
      locations."/".extraConfig = "try_files $uri /app.php$is_args$args;";
      locations."/assets".root = "${pkgs.wallabag}/app/web";
      locations."~ ^/app\\.php(/|$)".extraConfig = ''
        fastcgi_pass unix:${config.services.phpfpm.pools.wallabag.socket};
        include ${config.services.nginx.package}/conf/fastcgi.conf;
        fastcgi_param PATH_INFO $fastcgi_path_info;
        fastcgi_param PATH_TRANSLATED $document_root$fastcgi_path_info;
        fastcgi_param SCRIPT_FILENAME ${pkgs.wallabag}/web/$fastcgi_script_name;
        fastcgi_param DOCUMENT_ROOT ${pkgs.wallabag}/web;
        fastcgi_read_timeout 120;
        internal;
      '';
      locations."~ /(?!app)\\.php$".extraConfig = "return 404;";
    };

    # Set up PHP.
    services.phpfpm.pools.wallabag = {
      user = "wallabag";
      phpPackage = pkgs.php81.withExtensions ({
        enabled,
        all,
      }:
        enabled ++ [all.imagick all.tidy]);
      settings = {
        "env[WALLABAG_DATA]" = datadir;
        "listen.owner" = "nginx";
        "listen.group" = "nginx";
        "pm" = "dynamic";
        "pm.max_children" = 5;
        "pm.start_servers" = 2;
        "pm.min_spare_servers" = 1;
        "pm.max_spare_servers" = 3;
      };
      phpOptions = ''
        # Wallabag will crash on start-up.
        # https://github.com/wallabag/wallabag/issues/6042
        error_reporting = E_ALL & ~E_USER_DEPRECATED & ~E_DEPRECATED
      '';
    };

    # Write wallabag config
    system.activationScripts."wallabag-secret" = lib.stringAfter ["etc" "agenix"] ''
      secret=$(cat "${config.age.secrets.wallabag-secret.path}")
      configDir=/etc/wallabag
      mkdir -p "$configDir"
      configFile=$configDir/parameters.yml
      ${pkgs.gnused}/bin/sed "s#@secret@#$secret#" "${configFile}" > "$configFile"
      chown -R wallabag:nginx "$configDir"
      chmod 700 "$configDir"
      chmod 600 "$configFile"
    '';

    systemd.services.wallabag-install = {
      description = "Wallabag install service";
      wantedBy = ["multi-user.target"];
      before = ["phpfpm-bag.service"];
      path = [
        pkgs.coreutils
        config.services.phpfpm.pools.wallabag.phpPackage
        config.services.phpfpm.pools.wallabag.phpPackage.packages.composer
      ];
      serviceConfig = {
        User = "wallabag";
        Type = "oneshot";
        RemainAfterExit = "yes";
        PermissionsStartOnly = true;
      };
      preStart = ''
        mkdir -p "${datadir}"
        chown wallabag:nginx "${datadir}"
      '';
      script = ''
        echo "Setting up wallabag files in ${datadir} ..."
        cd "${datadir}"
        rm -rf var/cache/*
        rm -f app
        ln -sf "${appDir}" app
        ln -sf ${pkgs.wallabag}/composer.{json,lock} .
        export WALLABAG_DATA="${datadir}"
        if test ! -f installed; then
          mkdir -p data/db/
          php ${pkgs.wallabag}/bin/console --env=prod wallabag:install
          touch installed
        else
          php ${pkgs.wallabag}/bin/console --env=prod doctrine:migrations:migrate --no-interaction
        fi
        php ${pkgs.wallabag}/bin/console --env=prod cache:clear
      '';
    };

    # Include secret
    age.secrets.wallabag-secret = myData.ageModules.wallabag-secret;

    # Bind mount for persistent data for wallabag
    etu.base.zfs.system.directories = [
      "/var/lib/wallabag"
    ];
  };
}
