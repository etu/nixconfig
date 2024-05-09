{
  config,
  pkgs,
  myData,
  ...
}: {
  services.nextcloud.enable = true;
  services.nextcloud.package = pkgs.nextcloud29;
  services.nextcloud.config.adminuser = "etu";
  services.nextcloud.config.adminpassFile = config.age.secrets.nextcloud-admin-password.path;
  services.nextcloud.extraApps = {
    inherit (pkgs.nextcloud29Packages.apps) cookbook notes;
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
