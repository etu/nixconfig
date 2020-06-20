{ config, ... }:

{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "home.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      default = true;
      locations."/" = {
        root = "/persistent/var/www/home.elis.nu";
        extraConfig = "autoindex on;";
      };
    };
  };
}
