{ config, pkgs, ... }:

let
  # Import age secrets paths and metadata.
  ageModules = (import ../../../data.nix).ageModules;

in {
  # Make sure to have nginx enabled
  services.nginx.virtualHosts."freshrss.elis.nu" = {
    forceSSL = true;
    enableACME = true;
  };

  age.secrets = {
    inherit (ageModules) freshrss-password-etu;
  };

  # Set up freshrss.
  services.freshrss = {
    enable = true;
    baseUrl = "https://freshrss.elis.nu";
    virtualHost = "freshrss.elis.nu";

    # Set up my user
    defaultUser = "etu";
    passwordFile = config.age.secrets.freshrss-password-etu.path;
  };

  etu.base.zfs.system.directories = [
    # Bind mount for persistent data for freshrss
    "/var/lib/freshrss"
  ];
}
