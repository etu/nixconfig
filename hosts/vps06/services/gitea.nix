_: {
  etu.base.zfs.system.directories = [
    # Persistence of gitea data.
    "/var/lib/gitea"
  ];

  services.nginx.virtualHosts."git.elis.nu" = {
    forceSSL = true;
    enableACME = true;
    locations."/".proxyPass = "http://127.0.0.1:3000/";
  };

  services.postgresql = {
    ensureDatabases = ["gitea"];
    ensureUsers = [
      {
        name = "gitea";
        ensurePermissions."DATABASE gitea" = "ALL PRIVILEGES";
      }
    ];
  };

  services.gitea.enable = true;
  services.gitea.appName = "Elis Git Service";
  services.gitea.database.type = "postgres";
  services.gitea.settings.server.DOMAIN = "git.elis.nu";
  services.gitea.settings.server.ROOT_URL = "https://git.elis.nu/";
  services.gitea.settings.service.DISABLE_REGISTRATION = true;
  services.gitea.settings.session.COOKIE_SECURE = true;
}
