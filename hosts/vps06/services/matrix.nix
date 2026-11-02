{
  config,
  pkgs,
  lib,
  ...
}:
let
  # The server name is permanent: it's part of every user and room ID, so
  # changing it means starting over with a new database. The server itself
  # runs at matrix.${domain}, delegated from https://${domain}/.well-known/matrix/*
  # (served by the GitHub Pages blog at ${domain}).
  domain = "elis.nu";
in
{
  etu.base.zfs.system.directories = [
    # Persistence of synapse data between boots.
    "/var/lib/matrix-synapse"
    "/var/lib/matrix-appservice-irc"
  ];

  # Synapse needs a database with C collation, while ensureDatabases would
  # create it with the system locale. On a new host, create it by hand first:
  #   CREATE DATABASE "matrix-synapse" TEMPLATE template0
  #     LC_COLLATE 'C' LC_CTYPE 'C' ENCODING 'UTF8';
  services.postgresql = {
    ensureDatabases = [ "matrix-synapse" ];
    ensureUsers = [
      {
        name = "matrix-synapse";
        ensureDBOwnership = true;
      }
    ];
  };

  # Served through the Cloudflare tunnel, see ./cloudflared.nix.
  services.nginx.virtualHosts = {
    "element.${domain}" = {
      locations."/" = {
        root = pkgs.element-web.override (_: {
          conf = {
            default_server_config."m.homeserver" = {
              server_name = domain;
              base_url = "https://matrix.${domain}";
            };
            integrations_ui_url = "";
            integgrations_rest_url = "";
            integrations_widgets_urls = [ ];
            disable_guests = true;
            roomDirectory.servers = [
              domain
              "nixos.org"
              "matrix.org"
            ];
            features = {
              feature_pinning = "labs";
              feature_custom_status = "labs";
              feature_custom_tags = "labs";
              feature_state_counters = "labs";
            };
            showLabsSettings = true;
          };
        });
      };
    };
    "matrix.${domain}" = {
      locations = {
        "~* ^(\\/_matrix|\\/_synapse\\/client)" = {
          proxyPass = "http://[::1]:8448";
          # $remote_addr is the visitor's address (restored from
          # CF-Connecting-IP), and TLS ends at Cloudflare, so the scheme
          # nginx sees is always http.
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $remote_addr;
            proxy_set_header X-Forwarded-Proto https;
            proxy_read_timeout 900s;
          '';
        };
      };
    };
  };

  # `registration_shared_secret: …`, lets `matrix-synapse-register_new_matrix_user`
  # (run as root on this host) create accounts while registration is closed.
  age.secrets.matrix-synapse-registration = config.etu.data.ageModules.matrix-synapse-registration;

  services.matrix-synapse = {
    enable = true;
    extraConfigFiles = [ config.age.secrets.matrix-synapse-registration.path ];
    settings = {
      server_name = domain;
      public_baseurl = "https://matrix.${domain}";
      report_stats = true;
      enable_metrics = true;
      listeners = [
        {
          type = "metrics";
          port = 9148;
          bind_addresses = [ "127.0.0.1" ];
          resources = [ ];
          tls = false;
        }
        {
          bind_addresses = [ "::1" ];
          port = 8448;
          resources = [
            {
              compress = false;
              names = [ "client" ];
            }
            {
              compress = false;
              names = [ "federation" ];
            }
          ];
          tls = false;
          type = "http";
          x_forwarded = true;
        }
      ];
      trusted_key_servers = [
        {
          server_name = "matrix.org";
          verify_keys = {
            "ed25519:auto" = "Noi6WqcDj0QmPxCNQqgezwTlBKrfqehY1u2FyWP9uYw";
          };
        }
      ];
      redaction_retention_period = 1;
      rc_messages_per_second = 10;
      rc_message_burst_count = 15;
      key_refresh_interval = "1h"; # for initial setup so we can invalidate the key earlier
      max_upload_size = "10M";
      url_preview_enabled = false;
      dynamic_thumbnails = true; # might be a nicer user experience?
      allow_guest_access = false;
      enable_registration = false; # for admin purposes
      app_service_config_files = [
        "/var/lib/matrix-appservice-irc/registration.yml"
      ];
    };
  };

  # Hack to make the pre-start script not fail due to calling system
  # calls it's not allowed to call.
  systemd.services.matrix-appservice-irc.serviceConfig.SystemCallFilter = lib.mkForce "";

  services.matrix-appservice-irc = {
    enable = true;
    registrationUrl = "http://localhost:8009";

    settings = {
      # Talk to Synapse directly rather than through Cloudflare.
      homeserver.url = "http://[::1]:8448";
      homeserver.domain = domain;

      # Not sure where this value is used.
      ircService.mediaProxy.publicUrl = "https://matrix.${domain}";

      ircService.servers."cthulhu.irc.beanjuice.me" = {
        name = "beanjuice";
        port = 6697;
        ssl = true;
        sslselfsign = true;
        allowExpiredCerts = true;
        botConfig.enabled = false; # Don't send in a bridge bot
        dynamicChannels.enabled = true; # Allow dynamic joining of channels
        dynamicChannels.aliasTemplate = "#irc_$CHANNEL";
        ircClients.nickTemplate = "$DISPLAY"; # Nick display name: $DISPLAY[m]
        ircClients.allowNickChanges = true;

        # Connect all matrix users in the matrix room to the IRC channel on startup.
        membershipLists.enabled = true;
        membershipLists.global.matrixToIrc.initial = true;
        membershipLists.global.ircToMatrix.initial = true;
        membershipLists.global.ircToMatrix.requireMatrixJoined = true;
      };
    };
  };

  # use mimalloc to improve the memory situation with synapse
  systemd.services.matrix-synapse.environment = {
    SYNAPSE_CACHE_FACTOR = "1.0";
    LimitNOFILE = "4096";
  };
}
