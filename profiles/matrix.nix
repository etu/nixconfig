{ pkgs, ... }:

{
  # Cert renewal
  security.acme.certs = {
    "matrix.failar.nu" = {
      group = "matrix-synapse";
      allowKeysForGroup = true;
      postRun = "systemctl reload nginx.service; systemctl restart matrix-synapse.service";
    };
  };

  # Nginx
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "matrix.failar.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8008";
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  # Synapse
  services.matrix-synapse.enable = true;
  services.matrix-synapse.web_client = true;
  services.matrix-synapse.server_name = "failar.nu";
  services.matrix-synapse.database_type = "psycopg2";
  services.matrix-synapse.public_baseurl = "https://matrix.failar.nu/";
  # services.matrix-synapse.registration_shared_secret = "shared-secret-used-for-commandline-creation-of-users";

  services.matrix-synapse.tls_certificate_path = "/var/lib/acme/matrix.failar.nu/fullchain.pem";
  services.matrix-synapse.tls_private_key_path = "/var/lib/acme/matrix.failar.nu/key.pem";

  services.matrix-synapse.extraConfig = ''
    max_upload_size: "50M"
  '';

  services.matrix-synapse.listeners = [
    { bind_address = "127.0.0.1";
      port = 8008;
      resources = [
        { compress = true;  names = [ "client" "webclient" ]; }
        { compress = false; names = [ "federation" ]; }
      ];
      tls = false;
      type = "http";
      x_forwarded = true;
    }
  ];

  # Postgres
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql100;
  services.postgresql.dataDir = "/var/lib/postgresql/10.0";
}
