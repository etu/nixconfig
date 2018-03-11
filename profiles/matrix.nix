{ pkgs, ... }:

{
  # Caddy
  services.caddy.enable = true;
  services.caddy.agree = true;
  services.caddy.email = "elis@hirwing.se";
  services.caddy.config = ''
    matrix.failar.nu {
      gzip
      log syslog

      proxy / http://127.0.0.1:8008
    }
  '';

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  # Synapse
  services.matrix-synapse.enable = true;
  services.matrix-synapse.web_client = true;
  services.matrix-synapse.server_name = "failar.nu";
  services.matrix-synapse.database_type = "psycopg2";
  services.matrix-synapse.public_baseurl = "https://matrix.failar.nu/";
  # services.matrix-synapse.registration_shared_secret = "shared-secret-used-for-commandline-creation-of-users";

  services.matrix-synapse.extraConfig = ''
    max_upload_size: "50M"
  '';

  services.matrix-synapse.listeners = [
    { port = 8008;
      bind_address = "";
      type = "http";
      tls = false;
      x_forwarded = true;
      resources = [
          { names = [ "client" "webclient" ]; compress = true; }
          { names = [ "federation" ];         compress = false; }
      ];
    }
  ];

  # Postgres
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql100;
  services.postgresql.dataDir = "/var/lib/postgresql/10.0";
}
