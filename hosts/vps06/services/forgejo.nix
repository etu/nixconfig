{
  config,
  pkgs,
  ...
}:
{
  etu.base.zfs.system.directories = [
    # Persistence of forgejo data.
    "/var/lib/forgejo"
  ];

  # Served through the Cloudflare tunnel, see ./cloudflared.nix.
  services.nginx.virtualHosts."git.elis.nu" = {
    locations."/".proxyPass =
      "http://127.0.0.1:${builtins.toString config.services.forgejo.settings.server.HTTP_PORT}/";
    locations."/robots.txt".root = pkgs.writeTextDir "robots.txt" ''
      User-agent: *
      Disallow: /
    '';
  };

  services.postgresql = {
    ensureDatabases = [ "forgejo" ];
    ensureUsers = [
      {
        name = "forgejo";
        ensureDBOwnership = true;
      }
    ];
  };

  services.forgejo.enable = true;
  # Newer then LTS to work with gitea migration.
  services.forgejo.package = pkgs.forgejo;
  services.forgejo.database.type = "postgres";
  services.forgejo.settings.DEFAULT.APP_NAME = "Elis Git Service";
  services.forgejo.settings.server.DOMAIN = "git.elis.nu";
  services.forgejo.settings.server.ROOT_URL = "https://git.elis.nu/";
  services.forgejo.settings.server.HTTP_PORT = 3001;
  # git.elis.nu points to the Cloudflare tunnel, which only carries HTTP, so
  # SSH clone URLs use the host's own name, which still resolves to the VPS.
  services.forgejo.settings.server.SSH_DOMAIN = "vps06.elis.nu";
  services.forgejo.settings.service.DISABLE_REGISTRATION = true;
  services.forgejo.settings.service.REQUIRE_SIGNIN_VIEW = true;
  services.forgejo.settings.session.COOKIE_SECURE = true;
}
