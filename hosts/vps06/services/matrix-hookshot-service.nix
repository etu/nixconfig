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
  configFormat = pkgs.formats.yaml {};
  configFile = configFormat.generate "matrix-hookshot-config.yaml" cfg.config;
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

    config = let
      default = {
        bridge = {
          # Basic homeserver configuration
          domain = "example.com";
          url = "http://localhost:8008";
          mediaUrl = "https://example.com";
          port = cfg.registration.port;
          bindAddress = "127.0.0.1";
        };

        # A passkey used to encrypt tokens stored inside the bridge.
        passFile = passKeyFilePath;
      };
    in
      lib.mkOption {
        inherit default;
        inherit (configFormat) type;
        apply = lib.recursiveUpdate default;
        example = lib.literalExpression ''
          {
            # (Optional) Configure this to enable GitHub support
            github = {
              # Authentication for the GitHub App.
              auth = {
                id = 123;
                privateKeyFile = "github-key.pem";
              };

              # Webhook settings for the GitHub app.
              webhook = {
                secret = "secrettoken";
              };

              # (Optional) Settings for allowing users to sign in via OAuth.
              oauth = {
                client_id = "foo";
                client_secret = "bar";
                redirect_uri = "https://example.com/bridge_oauth/";
              };

              # (Optional) Default options for GitHub connections.
              defaultOptions = {
                showIssueRoomLink = false;
                hotlinkIssues = {
                  prefix = "#";
                };
              };

              # (Optional) Prefix used when creating ghost users for GitHub accounts.
              userIdPrefix = "_github_";
            };

            # (Optional) Configure this to enable GitLab support
            gitlab = {
              instances = {
                "gitlab.com" = {
                  url = "https://gitlab.com";
                };
              };

              webhook = {
                secret = "secrettoken";
                publicUrl = "https://example.com/hookshot/";
              };

              # (Optional) Prefix used when creating ghost users for GitLab accounts.
              userIdPrefix = "_gitlab_";
            };

            # (Optional) Configure this to enable Figma support
            figma = {
              publicUrl = "https://example.com/hookshot/";
              instances = {
                your-instance = {
                  teamId = "your-team-id";
                  accessToken = "your-personal-access-token";
                  passcode = "your-webhook-passcode";
                };
              };
            };

            # (Optional) Configure this to enable Jira support. Only
            # specify `url` if you are using a On Premise install
            # (i.e. not atlassian.com)
            jira = {
              # Webhook settings for JIRA
              webhook = {
                secret = "secrettoken";
              };

              # (Optional) OAuth settings for connecting users to
              # JIRA. See documentation for more information
              oauth = {
                client_id = "foo";
                client_secret = "bar";
                redirect_uri = "https://example.com/bridge_oauth/";
              };
            };

            # (Optional) Support for generic webhook events.
            #
            # 'allowJsTransformationFunctions' will allow users to write
            # short transformation snippets in code, and thus is unsafe
            # in untrusted environments
            generic = {
              enabled = false;
              enableHttpGet = false;
              urlPrefix = "https://example.com/webhook/";
              userIdPrefix = "_webhooks_";
              allowJsTransformationFunctions = false;
              waitForComplete = false;
            };

            # (Optional) Configure this to enable RSS/Atom feed support
            feeds = {
              enabled = false;
              pollIntervalSeconds = 600;
              pollTimeoutSeconds = 30;
            };

            # (Optional) Provisioning API for integration managers
            provisioning = {
              secret = "!secretToken";
            };

            # (Optional) Define profile information for the bot user
            bot = {
              displayname = "Hookshot Bot";
              avatar = "mxc://half-shot.uk/2876e89ccade4cb615e210c458e2a7a6883fe17d";
            };

            # (Optional) Define additional bot users for specific services
            serviceBots = [
              {
                localpart = "feeds";
                displayname = "Feeds";
                avatar = "mxc://half-shot.uk/2876e89ccade4cb615e210c458e2a7a6883fe17d";
                prefix = "!feeds";
                service = "feeds";
              }
            ];

            # (Optional) Prometheus metrics support
            metrics = {
              enabled = true;
            };

            # (Optional) Message queue / cache configuration options for
            # large scale deployments.  For encryption to work, must be
            # set to monolithic mode and have a host & port specified.
            queue = {
              monolithic = true;
              port = 6379;
              host = "localhost";
            };

            # (Optional) Logging settings. You can have a severity
            # debug,info,warn,error
            logging = {
              level = "info";
              colorize = true;
              json = false;
              timestampFormat = "HH:mm:ss:SSS";
            };

            # (Optional) EXPERIMENTAL support for complimentary widgets
            widgets = {
              addToAdminRooms = false;
              disallowedIpRanges = [
                "127.0.0.0/8"
                "10.0.0.0/8"
                "172.16.0.0/12"
                "192.168.0.0/16"
                "100.64.0.0/10"
                "192.0.0.0/24"
                "169.254.0.0/16"
                "192.88.99.0/24"
                "198.18.0.0/15"
                "192.0.2.0/24"
                "198.51.100.0/24"
                "203.0.113.0/24"
                "224.0.0.0/4"
                "::1/128"
                "fe80::/10"
                "fc00::/7"
                "2001:db8::/32"
                "ff00::/8"
                "fec0::/10"
              ];
              roomSetupWidget = {
                addOnInvite = false;
              };
              publicUrl = "https://example.com/widgetapi/v1/static/";
              branding = {
                widgetTitle = "Hookshot Configuration";
              };
            };

            # (Optional) Permissions for using the bridge. See
            # docs/setup.md#permissions for help
            permissions = [
              {
                actor = "example.com";
                services = [
                  {
                    service = "*";
                    level = "admin";
                  }
                ];
              }
            ];

            # (Optional) HTTP Listener configuration.
            # Bind resource endpoints to ports and addresses.
            #
            # 'port' must be specified. Each listener must listen on a
            # unique port.
            #
            # 'bindAddress' will default to '127.0.0.1' if not specified,
            # which may not be suited to Docker environments.
            #
            # 'resources' may be any of webhooks, widgets, metrics,
            # provisioning.
            listeners = [
              {
                port = 9000;
                bindAddress = "0.0.0.0";
                resources = [
                  "webhooks"
                ];
              }
              {
                port = 9001;
                bindAddress = "127.0.0.1";
                resources = [
                  "metrics"
                  "provisioning"
                ];
              }
              {
                port = 9002;
                bindAddress = "0.0.0.0";
                resources = [
                  "widgets"
                ];
              }
            ];
          }
        '';
        description = lib.mdDoc ''
          {file}`config.yaml` configuration as a Nix attribute set.
          Configuration options should match those described in
          [sample-configuration.yaml](https://matrix-org.github.io/matrix-hookshot/latest/setup/sample-configuration.html).
        '';
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

        # Write configuration file symlink.
        ln -sf ${configFile} ${configFilePath}
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
