{ config, pkgs, ... }:

let
  # Import age secrets paths and metadata.
  ageModules = (import ../../../data.nix).ageModules;
in {

  # Decrypt secret to expected location.
  age.secrets = {
    inherit (ageModules) cloudflare-api-env;
  };

  systemd.services.cloudflare-dyndns = {
    description = "Cloudflare dyndns updater";
    after = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeScript "cloudflare-dyndns" ''
        #!${pkgs.bash}/bin/bash
        set -eu

        source ${config.age.secrets.cloudflare-api-env.path}

        export CURRENT_IP_ADDRESS="$(${pkgs.curl}/bin/curl -s4 ip.failar.nu)"
        ${pkgs.coreutils}/bin/echo "Current IP is: $CURRENT_IP_ADDRESS"

        export CURRENT_DNS_VALUE=$(${pkgs.curl}/bin/curl -s -X GET "https://api.cloudflare.com/client/v4/zones/$DNS_ZONE/dns_records/$DNS_RECORD" -H "Authorization: Bearer $AUTH_KEY" -H "Content-Type:application/json" | ${pkgs.jq}/bin/jq -r '.result["content"]')
        ${pkgs.coreutils}/bin/echo "Current DNS value is: $CURRENT_DNS_VALUE"

        if test "$CURRENT_DNS_VALUE" != "$CURRENT_IP_ADDRESS"; then
          ${pkgs.coreutils}/bin/echo "Updating DNS record"
          ${pkgs.curl}/bin/curl -s -X PUT "https://api.cloudflare.com/client/v4/zones/$DNS_ZONE/dns_records/$DNS_RECORD" -H "Authorization: Bearer $AUTH_KEY" -H "Content-Type:application/json" --data '{"type":"A","name":"$DNS_RECORD_NAME","content":"$CURRENT_IP_ADDRESS"}'
        else
          ${pkgs.coreutils}/bin/echo "No update needed"
        fi
      '';
    };
  };
}
