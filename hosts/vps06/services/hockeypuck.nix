{...}: {
  etu.base.zfs.system.directories = [
    # Persistence of hockeypuck data.
    "/var/lib/hockeypuck"
  ];

  services.nginx.virtualHosts."keys.proxxi.org" = {
    forceSSL = true;
    enableACME = true;
    locations."/".proxyPass = "http://127.0.0.1:11371/";
  };

  services.postgresql = {
    ensureDatabases = ["hockeypuck"];
    ensureUsers = [
      {
        name = "hockeypuck";
        ensurePermissions."DATABASE hockeypuck" = "ALL PRIVILEGES";
      }
    ];
  };

  # Open Firewall for hkp (keyserver)
  networking.firewall.allowedTCPPorts = [11371];

  # Enable hockeypuck keyserver
  services.hockeypuck.enable = true;
}
