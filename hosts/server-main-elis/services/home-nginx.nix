{ config, pkgs, ... }:
{
  # Make sure to have nginx enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."misc.elis.nu" = {
    default = true;
    root = "${config.etu.dataPrefix}/var/www/misc.elis.nu";

    locations."/".extraConfig = "autoindex on;";
    locations."/robots.txt".root = pkgs.writeTextDir "robots.txt" ''
      User-agent: *
      Disallow: /
    '';
  };
}
