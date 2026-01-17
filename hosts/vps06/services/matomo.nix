{
  pkgs,
  ...
}:
{
  etu.base.zfs.system.directories = [
    # Bind mount for persistent data for matomo
    "/var/lib/matomo"
    "/var/lib/mysql"
  ];

  # Enable matomo
  services.matomo.enable = true;
  services.matomo.hostname = "matomo.elis.nu";
  services.matomo.nginx = { };

  # Enable mariadb
  services.mysql.enable = true;
  services.mysql.package = pkgs.mariadb;
  services.mysql.ensureDatabases = [ "matomo" ];
  services.mysql.ensureUsers = [
    {
      name = "matomo";
      ensurePermissions."matomo.*" = "ALL PRIVILEGES";
    }
  ];

  # Optimize mariadb to run on ZFS.
  services.mysql.settings.mysqld = {
    innodb_flush_method = "fsync";
    innodb_doublewrite = 0; # ZFS is transactional
    innodb_use_native_aio = 0;
    innodb_read_io_threads = 10;
    innodb_write_io_threads = 10;
  };
}
