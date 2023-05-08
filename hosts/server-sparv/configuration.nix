# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  lib,
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
    stateVersion = "23.05";

    # Set data prefix so agenix can find the host keys.
    dataPrefix = "/";

    # We do have a persistent root on this system, I know, it's
    # weird. So here I disable the persstence settings for this system
    # and just keep the files on /
    base.zfs.enable = lib.mkForce false;

    base.nix.allowUnfree = ["minecraft-server"];

    base.emacs.enable = lib.mkForce false;
    base.telegraf.enable = true;
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/root".use_template = ["data"];
      "zroot/safe/home".use_template = ["data"];
      "zroot/local/minecraft".use_template = ["data"]; # Minecraft server
    };
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Allow inbound traffic to lancache ports.
  networking.firewall.allowedTCPPorts = [80 443];
  networking.firewall.allowedUDPPorts = [53];

  # Set up docker.
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up lancache docker container.
  virtualisation.oci-containers.backend = "docker";
  virtualisation.oci-containers.containers = {
    lancache = {
      image = "lancachenet/monolithic:latest";
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
      image = "lancachenet/lancache-dns:latest";
      ports = [
        "53:53/udp"
      ];
      environment = {
        LANCACHE_IP = "10.69.0.3";
        USE_GENERIC_CACHE = "true";
        UPSTREAM_DNS = "1.1.1.2";
      };
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

      # Enable to enable white list "username" = "uuid" to allow users
      # to connect.
      white-list = false;

      # Disable spawn protection.
      spawn-protection = 0;

      # Enable password to allow to connect.
      # enable-rcon = true;
      # "rcon.password" = "hunter2";
    };

    # Enable to allow users to connect.
    # Example:
    # - "username" = "uuid"
    # whitelist = {};
  };
}
