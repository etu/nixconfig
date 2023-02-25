{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.matrix-hookshot;

  # Config file paths
  registrationFilePath = "/var/lib/matrix-hookshot/registration.yaml";
  configFilePath = "/var/lib/matrix-hookshot/config.yaml";
  passKeyFilePath = "/var/lib/matrix-hookshot/passkey.pem";

  # registration.yml file contents
  registrationJson = {
    url = cfg.registration.registrationUrl;
    # This is a trick to shell out to pwgen in the shell script
    # to generate these three identifiers. Then we have shell
    # script safeguards to restore the previously set values on
    # updates.
    id = "'$(${pkgs.pwgen}/bin/pwgen -s 64 -c 1)'";
    as_token = "'$(${pkgs.pwgen}/bin/pwgen -s 64 -c 1)'";
    hs_token = "'$(${pkgs.pwgen}/bin/pwgen -s 64 -c 1)'";
    sender_localpart = cfg.registration.localpart;
    namespaces = cfg.registration.namespaces;
  };

  # config.yml file contents
  configJson = {
    # Basic homeserver configuration.
    bridge =
      {
        port = cfg.registration.port;
      }
      // cfg.config.bridge;

    # Path to a passkey used to encrypt tokens stored inside the bridge.
    passFile = passKeyFilePath;

    # Support for generic webhook events.
    generic = cfg.config.generic;

    # Logging settings.
    logging = cfg.config.logging;

    # Prometheus metrics support.
    metrics = cfg.config.metrics;

    # Permissions for using the bridge.
    permissions = cfg.config.permissions;

    # HTTP Listener configuration
    listeners = cfg.config.listeners;
  };
in {
  options.services.matrix-hookshot = {
    enable = lib.mkEnableOption (lib.mdDoc "the Matrix webhook integration");

    registration = {
      port = lib.mkOption {
        type = lib.types.port;
        description = lib.mdDoc "The port to listen on";
        default = 9993;
      };

      registrationUrl = lib.mkOption {
        type = lib.types.str;
        description = lib.mdDoc ''
          The URL where the application service is listening for homeserver requests,
          from the Matrix homeserver perspective.
        '';
        default = "http://localhost:${toString cfg.registration.port}";
      };

      localpart = lib.mkOption {
        type = lib.types.str;
        description = lib.mdDoc "The user_id localpart to assign to the appservice";
        default = "hookshot";
      };

      namespaces = lib.mkOption {
        type = lib.types.attrs;
        description = lib.mdDoc "namespaces settings for registration.yml";
        example = {
          users = [
            {
              exclusive = true;
              regex = "^@webhook_.*:example.org";
            }
          ];
        };
      };
    };

    config = {
      bridge = lib.mkOption {
        type = lib.types.attrs;
        description = lib.mdDoc "Basic homeserver configuration, port is inherited from `services.matrix-hookshot.registration.port` but can be overridden here.";
        example = {
          domain = "example.org";
          url = "http://localhost:8008";
          mediaUrl = "https://example.org";
          bindAddress = "127.0.0.1";
        };
      };

      metrics = lib.mkOption {
        type = lib.types.attrs;
        description = lib.mdDoc "Prometheus metrics support";
        default.enabled = true;
      };

      logging = lib.mkOption {
        type = lib.types.attrs;
        description = lib.mdDoc "Logging settings. You can have a severity debug,info,warn,error";
        default = {
          level = "info";
          colorize = true;
          json = false;
          timestampFormat = "HH:mm:ss:SSS";
        };
      };

      generic = lib.mkOption {
        type = lib.types.attrs;
        description = lib.mdDoc "Support for generic webhooks";
        default = {
          enabled = false;
          enableHttpGet = false;
          urlPrefix = "https://example.com/webhook/";
          userIdPrefix = "_webhooks_";
          allowJsTransformationFunctions = false;
          waitForComplete = false;
        };
      };

      permissions = lib.mkOption {
        type = lib.types.listOf lib.types.attrs;
        description = lib.mdDoc "Permissions for using the bridge.";
        example = [
          {
            actor = "example.org";
            services = [
              {
                service = "*";
                level = "admin";
              }
            ];
          }
        ];
      };

      listeners = lib.mkOption {
        type = lib.types.listOf lib.types.attrs;
        description = lib.mdDoc "HTTP Listener configuration";
        example = [
          {
            port = 9000;
            bindAddress = "0.0.0.0";
            resources = ["webhooks"];
          }
          {
            port = 9001;
            bindAddress = "127.0.0.1";
            resources = ["metrics" "provisioning"];
          }
          {
            port = 9002;
            bindAddress = "0.0.0.0";
            resources = ["widgets"];
          }
        ];
      };
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.matrix-hookshot = {
      description = "Matrix-hookshot bridge";
      before = ["matrix-synapse.service"]; # So the registration can be used by Synapse
      wantedBy = ["multi-user.target"];
      after = ["network.target"];

      preStart = ''
        umask 077

        # Generate key for encrypting things
        if ! [ -f "${passKeyFilePath}" ]; then
          ${pkgs.openssl}/bin/openssl genpkey \
            -out "${passKeyFilePath}" \
            -outform PEM \
            -algorithm RSA \
            -pkeyopt rsa_keygen_bits:4096
        fi

        # Generate registration config file
        if ! [ -f "${registrationFilePath}" ]; then
          # The easy case: Just write the file.
          echo '${builtins.toJSON registrationJson}' | ${pkgs.yq}/bin/yq -y > ${registrationFilePath}
        else
          # The tricky case: The file has already been generated.

          # Backup id, hs_token and as_token.
          id=$(grep "^id:.*$" ${registrationFilePath})
          hs_token=$(grep "^hs_token:.*$" ${registrationFilePath})
          as_token=$(grep "^as_token:.*$" ${registrationFilePath})

          # Write out registration file.
          echo '${builtins.toJSON registrationJson}' | ${pkgs.yq}/bin/yq -y > ${registrationFilePath}

          # Restore registration config file values.
          ${pkgs.gnused}/bin/sed -i "s/^id:.*$/$id/g" ${registrationFilePath}
          ${pkgs.gnused}/bin/sed -i "s/^hs_token:.*$/$hs_token/g" ${registrationFilePath}
          ${pkgs.gnused}/bin/sed -i "s/^as_token:.*$/$as_token/g" ${registrationFilePath}
        fi

        # Allow synapse access to the registration
        if ${lib.getBin pkgs.glibc}/bin/getent group matrix-synapse > /dev/null; then
          chown matrix-hookshot:matrix-synapse ${registrationFilePath}
          chmod 640 ${registrationFilePath}
        fi

        # Write the normal config file.
        echo '${builtins.toJSON configJson}' | ${pkgs.yq}/bin/yq -y > ${configFilePath}
      '';

      script = "${pkgs.matrix-hookshot}/bin/matrix-hookshot ${configFilePath} ${registrationFilePath}";
      serviceConfig = {
        User = "matrix-hookshot";
        Group = "matrix-hookshot";
        StateDirectory = "matrix-hookshot";
        WorkingDirectory = "/var/lib/matrix-hookshot";

        # Add permissions to chown the registration file
        CapabilityBoundingSet = ["CAP_CHOWN"];
        AmbientCapabilities = ["CAP_CHOWN"];
      };
    };

    users.groups.matrix-hookshot = {};
    users.users.matrix-hookshot = {
      group = "matrix-hookshot";
      home = "/var/lib/matrix-hookshot";
      isSystemUser = true;
    };
  };
}
