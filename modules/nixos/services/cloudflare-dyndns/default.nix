{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) mkOption types;
in
{
  options.etu.services.cloudflare-dyndns = {
    enable = mkOption {
      default = false;
      type = types.bool;
      description = "Enable Cloudflare DynDNS service";
    };

    secretName = mkOption {
      type = types.str;
      description = "Name of the age secret containing Cloudflare API credentials (e.g., 'cloudflare-api-env')";
      example = "cloudflare-api-env";
      longDescription = ''
        The secret file should contain environment variables:

        - DNS_ZONE: Cloudflare Zone ID
        - DNS_RECORD: Cloudflare DNS Record ID
        - AUTH_KEY: Cloudflare API Token with DNS edit permissions
        - DNS_RECORD_NAME: The DNS record name to update (e.g., "home.example.com")
      '';
    };

    schedule = mkOption {
      type = types.str;
      default = "hourly";
      description = "systemd timer schedule for running the DynDNS update";
      example = "hourly";
    };

    ipCheckUrl = mkOption {
      type = types.str;
      default = "https://ip.failar.nu";
      description = "URL to use for obtaining the current public IP address";
    };
  };

  config = lib.mkIf config.etu.services.cloudflare-dyndns.enable (
    let
      cfg = config.etu.services.cloudflare-dyndns;

      secretPath = config.age.secrets.${cfg.secretName}.path;

      cloudflare-dyndns = pkgs.writeShellApplication {
        name = "cloudflare-dyndns";
        runtimeInputs = with pkgs; [
          curl
          jq
        ];
        text = ''
          # Defaults (overridden by sourced secret env, if set there)
          : "''${DNS_ZONE:=CHANGEME_DNS_ZONE}"
          : "''${DNS_RECORD:=CHANGEME_DNS_RECORD_ID}"
          : "''${AUTH_KEY:=CHANGEME_AUTH_TOKEN}"
          : "''${DNS_RECORD_NAME:=CHANGEME_RECORD_NAME}"

          # Override from secret file (should export vars)
          # shellcheck disable=SC1091
          source "${secretPath}"

          CURRENT_IP_ADDRESS="$(curl -s4 "${cfg.ipCheckUrl}")"
          echo "Current IP is: $CURRENT_IP_ADDRESS"

          CURRENT_DNS_VALUE="$(
            curl -s -X GET "https://api.cloudflare.com/client/v4/zones/$DNS_ZONE/dns_records/$DNS_RECORD" \
              -H "Authorization: Bearer $AUTH_KEY" \
              -H "Content-Type:application/json" \
            | jq -r '.result["content"]'
          )"
          echo "Current DNS value is: $CURRENT_DNS_VALUE"

          if test "$CURRENT_DNS_VALUE" != "$CURRENT_IP_ADDRESS"; then
            echo "Updating DNS record"
            curl -s -X PUT "https://api.cloudflare.com/client/v4/zones/$DNS_ZONE/dns_records/$DNS_RECORD" \
              -H "Authorization: Bearer $AUTH_KEY" \
              -H "Content-Type:application/json" \
              --data "{\"type\":\"A\",\"name\":\"$DNS_RECORD_NAME\",\"content\":\"$CURRENT_IP_ADDRESS\"}"
          else
            echo "No update needed"
          fi
        '';
      };
    in
    {
      age.secrets."${cfg.secretName}" = config.etu.data.ageModules.${cfg.secretName};

      systemd.services.cloudflare-dyndns = {
        description = "Cloudflare DynDNS updater";
        after = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        requires = [ "network-online.target" ];
        startAt = cfg.schedule;
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${cloudflare-dyndns}/bin/cloudflare-dyndns";
        };
      };
    }
  );
}
