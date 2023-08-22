{via-elis-nu, ...}: {
  # Enable the ip-failar-nu service
  services.ip-failar-nu.enable = true;

  services.nginx.virtualHosts = {
    "ip.failar.nu" = {
      addSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:8123/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };
    "sa.0b.se" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "https://elis.nu/";
    };
    "sa4b.se" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "https://elis.nu/";
    };
    "via.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      locations."/".root = via-elis-nu;
    };
  };
}
