{
  config,
  pkgs,
  myData,
  ...
}: {
  services.nextcloud.enable = true;
  services.nextcloud.package = pkgs.nextcloud30;
  services.nextcloud.config.adminuser = "etu";
  services.nextcloud.config.adminpassFile = config.age.secrets.nextcloud-admin-password.path;
  services.nextcloud.extraApps = {
    inherit (pkgs.nextcloud30Packages.apps) cookbook notes;
  };
  services.nextcloud.hostName = "nextcloud.elis.nu";
  services.nginx.enable = true;

  # Import secret
  age.secrets = {
    inherit (myData.ageModules) nextcloud-admin-password;
  };

  etu.base.zfs.system.directories = [
    # Bind mount for persistent data for freshrss
    "/var/lib/nextcloud"
  ];
}
