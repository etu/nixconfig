# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  myData,
  pkgs,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix
  ];

  # Set hostname
  networking.hostName = "zparv-zerver";

  # Settings needed for ZFS
  networking.hostId = "48a0ce30";

  # Enable weekly garbage-collection and daily store optimization.
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
  nix.optimise.automatic = true;
  nix.optimise.dates = ["daily"];

  # My module settings
  etu = {
    stateVersion = "24.05";

    # Set data prefix so agenix can find the host keys.
    dataPrefix = "/";

    # We do have a persistent root on this system, I know, it's
    # weird. So here I disable the persstence settings for this system
    # and just keep the files on /
    base.zfs.enable = false; # Disable emacs that is enabled by default.

    base.nix.allowUnfree = ["minecraft-server"];

    base.emacs.enable = false; # Disable emacs that is enabled by default.
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/root".use_template = ["data"];
      "zroot/safe/home".use_template = ["data"];
      "zroot/local/minecraft".use_template = ["data"]; # Minecraft server
      "zroot/local/valheim".use_template = ["data"]; # Valheim server
    };

    # Allow github to deploy system
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.github-actions;

    services.netdata.enable = true;
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Allow inbound traffic to lancache ports.
  networking.firewall.allowedTCPPorts = [80 443];
  networking.firewall.allowedUDPPorts = [
    # DNS
    53

    # Valheim
    2456
    2457
  ];

  # Set up docker.
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up lancache docker container.
  virtualisation.oci-containers.backend = "docker";
  virtualisation.oci-containers.containers = {
    lancache = {
      image = "docker.io/lancachenet/monolithic:latest";
      ports = [
        "80:80/tcp"
        "443:443/tcp"
      ];
      volumes = [
        "/media/zstorage/lancache/data:/data/cache"
        "/media/zstorage/lancache/logs:/data/logs"
      ];
    };
    lancache-dns = {
      image = "docker.io/lancachenet/lancache-dns:latest";
      ports = [
        "53:53/udp"
      ];
      environment = {
        LANCACHE_IP = "10.69.0.3";
        USE_GENERIC_CACHE = "true";
        UPSTREAM_DNS = "1.1.1.2";
      };
    };

    # Set up a valheim server
    valheim-server = {
      image = "ghcr.io/lloesche/valheim-server:latest";
      ports = [
        "2456-2457:2456-2457/udp"
      ];
      environmentFiles = [config.age.secrets.valheim-server-env.path];
      volumes = [
        "/var/lib/valheim/config:/config"
        "/var/lib/valheim/data:/opt/valheim"
      ];
    };
  };

  # Include secret
  age.secrets.valheim-server-env = myData.ageModules.valheim-server-env;
  age.secrets.enshrouded-server-env = myData.ageModules.enshrouded-server-env;

  # Restart valheim service every day
  systemd.services.restart-valheim-service = {
    description = "Restart valheim service";
    after = ["docker.service"];
    serviceConfig.Type = "simple";
    script = "${pkgs.systemd}/bin/systemctl restart docker-valheim-server.service";
  };
  systemd.timers.restart-valheim-service = {
    wantedBy = ["timers.target"];
    after = ["docker.service"];
    timerConfig = {
      OnCalendar = "05:15";
      Persistent = "yes";
    };
  };

  # Set up a minecraft server
  services.minecraft-server = {
    enable = true;
    eula = true;
    openFirewall = true;
    declarative = true;

    serverProperties = {
      server-port = 25565;
      motd = "Välkommen till Sparv's Minecraft server!";
      difficulty = "normal";
      gamemode = "survival";
      max-players = 10;
      snooper-enabled = false;

      enable-command-block = true;
      online-mode = true;
      white-list = true;

      # Disable spawn protection.
      spawn-protection = 0;

      # Enable password to allow to connect.
      # enable-rcon = true;
      # "rcon.password" = "hunter2";
    };

    # Enable to allow users to connect.
    # Example:
    # - "username" = "uuid"
    whitelist = {
      "etuetuetu" = "e5520c26-81b6-4683-9ed0-53dc8b5f4d3f";
      "Angrontyr" = "5a88e4f6-56c1-4f16-93d7-34bd6aea3d74";
      "PikabooSuprise" = "3357057f-77ee-45b5-a0c5-5f5b303b0e02";
      "Eiydra" = "47fa8f5f-f7d3-441a-8c12-4306372ee81b";
      "LightCatcher0_0" = "b1a5906a-2f2e-4a51-b273-2b0e164eafde";
      "Steelwolf16" = "e0000732-519e-46ac-9d1c-b4b2f460534f";
      "concate" = "776ddc3d-f182-4b8c-a07e-ae4b355994d8";
      "ScarletHunter22" = "1cf53c7c-75c3-480a-b91a-8a64fe84a3cc";
      "Pralin" = "72343c63-2442-4333-8e97-72c98d691905";
      "gondelix" = "7e095af1-c96a-408d-ad22-334c64db9700";
    };
  };
}
