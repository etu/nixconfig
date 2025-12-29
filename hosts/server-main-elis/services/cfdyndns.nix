{ config, pkgs, ... }:
let
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
      source "${config.age.secrets.cloudflare-api-env.path}"

      CURRENT_IP_ADDRESS="$(curl -s4 ip.failar.nu)"
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
  age.secrets = {
    inherit (config.etu.data.ageModules) cloudflare-api-env;
  };

  systemd.services.cloudflare-dyndns = {
    description = "Cloudflare dyndns updater";
    after = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    requires = [ "network-online.target" ];
    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${cloudflare-dyndns}/bin/cloudflare-dyndns";
    };
  };
}
