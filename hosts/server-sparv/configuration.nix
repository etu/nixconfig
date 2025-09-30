# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
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
      "zroot/safe/minecraft".use_template = ["data"]; # Minecraft server
      "zroot/safe/valheim-saves".use_template = ["data"]; # Valheim server
      "zroot/local/project-zomboid".use_template = ["data"]; # Project Zomboid server
      "zroot/local/vrising".use_template = ["data"]; # V Rising server
    };

    user.extraRootAuthorizedKeys =
      # Allow home server to pull backups
      config.etu.data.pubkeys.etu.syncoid.server-main-elis
      ++
      # Allow github to deploy system
      config.etu.data.pubkeys.etu.github-actions;

    services.netdata.enable = true;

    # Allow beszel to monitor this system
    services.beszel-agent.enable = true;
    services.beszel-agent.extraFilesystems = [
      "/boot"
      "/boot-fallback"
      "/data"
      "/data/local"
      "/home"
      #"/media/zstorage"
      "/nix"
    ];
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Set cpupower governor to performance.
  powerManagement.cpuFreqGovernor = "performance";

  # Set up docker.
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up lancache docker container.
  virtualisation.oci-containers.backend = "docker";
  virtualisation.oci-containers.containers = {
    #lancache = {
    #  image = "docker.io/lancachenet/monolithic:latest";
    #  ports = [
    #    "80:80/tcp"
    #    "443:443/tcp"
    #  ];
    #  volumes = [
    #    "/media/zstorage/lancache/data:/data/cache"
    #    "/media/zstorage/lancache/logs:/data/logs"
    #  ];
    #  extraOptions = [
    #    "--ulimit"
    #    "nofile=1048576:1048576"
    #  ];
    #};
    #lancache-dns = {
    #  image = "docker.io/lancachenet/lancache-dns:latest";
    #  ports = [
    #    "53:53/udp"
    #  ];
    #  environment = {
    #    LANCACHE_IP = "10.69.0.3";
    #    USE_GENERIC_CACHE = "true";
    #    UPSTREAM_DNS = "1.1.1.2";
    #  };
    #};

    # Set up a valheim server
    valheim-server = {
      image = "ghcr.io/mbround18/valheim:latest";
      ports = [
        "2456-2458:2456-2458/udp"
      ];
      environment = {
        PORT = "2456";
        PUBLIC = "0";
        ENABLE_CROSSPLAY = "1";

        # Disable updates
        AUTO_UPDATE = "0";
        UPDATE_ON_STARTUP = "0";

        # Enable mods
        TYPE = "BepInEx";
        MODS = ''
          CW_Jesse-BetterNetworking_Valheim-2.3.2
          mvp-Serverside_Simulations-1.1.9
        '';
      };
      environmentFiles = [config.age.secrets.valheim-server-env.path];
      volumes = [
        "/var/lib/valheim/backups:/home/steam/backups"
        "/var/lib/valheim/saves:/home/steam/.config/unity3d/IronGate/Valheim"
        "/var/lib/valheim/server:/home/steam/valheim"
      ];
    };

    # Set up a project zomboid server
    project-zomboid = {
      image = "docker.io/danixu86/project-zomboid-dedicated-server:latest";
      ports = [
        "8766-8767:8766-8767/udp"
        "16261:16261/udp"
        "16262-16272:16262-16272/tcp"
      ];
      environmentFiles = [config.age.secrets.project-zomboid-env.path];
      volumes = [
        "/var/lib/project-zomboid:/home/steam/Zomboid"
      ];
    };

    vrising-server = {
      image = "docker.io/trueosiris/vrising:latest";
      ports = [
        "9876-9877:9876-9877/udp"
      ];
      environment = {
        TZ = "Europe/Stockholm";
        SERVERNAME = "SparvRising";
      };
      volumes = [
        "/var/lib/vrising/server:/mnt/vrising/server:rw"
        "/var/lib/vrising/persistentdata:/mnt/vrising/persistentdata:rw"
      ];
    };
  };

  # Include secret
  age.secrets.valheim-server-env = config.etu.data.ageModules.valheim-server-env;
  age.secrets.project-zomboid-env = config.etu.data.ageModules.project-zomboid-env;

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
      "Slysor" = "e370b6b0-a470-4367-8d1c-51cf31432898";
      "M1TTEN5" = "75e6ab27-5fb1-4fc3-aafb-c6f00c0d98e7";
    };
  };
}
