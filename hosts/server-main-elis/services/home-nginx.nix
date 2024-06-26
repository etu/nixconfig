{
  config,
  pkgs,
  ...
}: {
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "home.elis.nu" = {
      forceSSL = true;
      enableACME = true;
      default = true;
      root = "${config.etu.dataPrefix}/var/www/home.elis.nu";

      locations."/" = {
        extraConfig = "autoindex on;";
      };
      locations."/robots.txt".root = pkgs.writeTextDir "robots.txt" ''
        User-agent: *
        Disallow: /
      '';
    };
  };
}
