{ config, ... }:

{
  # Make sure to have NGiNX enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."local.elis.nu".locations."/changedetection/" = {
    proxyPass = "http://127.0.0.1:5000/";
    extraConfig = ''
        allow 192.168.0.0/24;
        deny all;
        proxy_set_header Host "local.elis.nu";
        proxy_set_header X-Forwarded-Prefix /changedetection;
    '';
  };

  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers.changedetection = {
    image = "dgtlmoon/changedetection.io:0.39.7";
    ports = [ "0.0.0.0:5000:5000" ];
    volumes = [
      "/persistent/var/lib/changedetection:/datastore"
    ];
    # Make the software listen to headers to work in a subpath behind
    # a proxy.
    environment.USE_X_SETTINGS = "1";
  };
}
