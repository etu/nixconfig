{
  config,
  pkgs,
  myData,
  ...
}: {
  services.nextcloud.enable = true;
  services.nextcloud.package = pkgs.nextcloud27;
  services.nextcloud.config.adminuser = "etu";
  services.nextcloud.config.adminpassFile = config.age.secrets.nextcloud-admin-password.path;
  # services.nextcloud.home = "/var/lib/nextcloud"; # default value
  services.nextcloud.extraApps = {
    inherit (pkgs.nextcloud27Packages.apps) cookbook news;
  };
  services.nextcloud.hostName = "nextcloud.elis.nu";
  services.nextcloud.https = true;

  services.nginx.enable = true;
  services.nginx.virtualHosts.${config.services.nextcloud.hostName} = {
    forceSSL = true;
    enableACME = true;
  };

  # Import secret
  age.secrets = {
    inherit (myData.ageModules) nextcloud-admin-password;
  };

  etu.base.zfs.system.directories = [
    # Bind mount for persistent data for freshrss
    "/var/lib/nextcloud"
  ];
}
