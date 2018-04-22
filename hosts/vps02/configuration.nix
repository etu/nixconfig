# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  coturn-ports = [ 3478 3479 5349 5350 ];
  http-ports = [ 80 443 ];
  coturn-shared-secret = builtins.readFile /var/lib/coturn-shared-secret.key;

in {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ../../profiles/common-server.nix
  ];

  networking.hostName = "vps02";

  # Set up bootloader.
  boot.loader.grub.device = "/dev/vda";
  boot.cleanTmpDir = true;


  # Set up groups.
  users.groups.matrix-tls = {
    members = [ "matrix-synapse" "turnserver" ];
  };

  # Cert setup and renewal.
  security.acme.certs = {
    "matrix.failar.nu" = {
      group = "matrix-tls";
      allowKeysForGroup = true;
      postRun = "systemctl reload nginx.service; systemctl restart matrix-synapse.service";
    };
  };

  # Nginx proxy.
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

  # Synapse.
  services.matrix-synapse.enable = true;
  services.matrix-synapse.web_client = true;
  services.matrix-synapse.server_name = "failar.nu";
  services.matrix-synapse.database_type = "psycopg2";
  services.matrix-synapse.public_baseurl = "https://matrix.failar.nu/";
  # services.matrix-synapse.registration_shared_secret = "shared-secret-used-for-commandline-creation-of-users";
  services.matrix-synapse.turn_shared_secret = coturn-shared-secret;
  services.matrix-synapse.turn_uris = [
    "turn:matrix.failar.nu:3478?transport=udp"
    "turn:matrix.failar.nu:3478?transport=tcp"
  ];

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

  # Postgres.
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql100;
  services.postgresql.dataDir = "/var/lib/postgresql/10.0";

  # Coturn.
  services.coturn.enable = true;
  services.coturn.lt-cred-mech = true;
  services.coturn.use-auth-secret = true;
  services.coturn.static-auth-secret = coturn-shared-secret;
  services.coturn.realm = "failar.nu";
  services.coturn.cert = "/var/lib/acme/matrix.failar.nu/fullchain.pem";
  services.coturn.pkey = "/var/lib/acme/matrix.failar.nu/key.pem";

  # Firewall things.
  networking.firewall.allowedTCPPorts = http-ports ++ coturn-ports;
  networking.firewall.allowedUDPPorts = coturn-ports;
}
