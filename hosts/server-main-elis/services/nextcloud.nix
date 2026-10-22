{
  pkgs,
  ...
}:
{
  services.nextcloud.enable = true;
  services.nextcloud.package = pkgs.nextcloud35;
  services.nextcloud.config.dbtype = "sqlite";
  services.nextcloud.extraApps = {
    inherit (pkgs.nextcloud35Packages.apps) cookbook notes forms;
  };
  services.nextcloud.hostName = "nextcloud.elis.nu";
  services.nginx.enable = true;

  etu.base.zfs.system.directories = [
    # Bind mount for persistent data for freshrss
    "/var/lib/nextcloud"
  ];
}
