# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  flake,
  pkgs,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Some networking wait tooks for certain services
    ./network-wait-hook.nix

    # Import local services that are host specific
    ./services/cfdyndns.nix
    ./services/empty-dirs-cleaner.nix
    ./services/hass.nix
    ./services/home-nginx.nix
    ./services/homepage.nix
    ./services/nextcloud.nix
    ./services/svtplay.nix
    ./services/usenet.nix

    # Import my default modules
    flake.nixosModules.default
  ];

  # Set hostname
  networking.hostName = "server-main-elis";

  # Enable networking.
  networking.useDHCP = true;

  # Settings needed for ZFS
  networking.hostId = "23916528";

  # Enable weekly garbage-collection and daily store optimization.
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
  nix.optimise.automatic = true;
  nix.optimise.dates = ["daily"];

  # My module settings
  etu = {
    stateVersion = "24.05";

    base.emacs.enable = false; # Disable emacs that is enabled by default.
    development.git.enable = true;
    user.enable = true;
    user.extraGroups = ["libvirtd"];

    user.extraRootAuthorizedKeys =
      # Allow workstations to push snapshots
      config.etu.data.pubkeys.etu.syncoid.workstations
      ++
      # Allow github to deploy system
      config.etu.data.pubkeys.etu.github-actions;

    services.freshrss.enable = true;
    services.jellyfin.enable = true;
    services.netdata.enable = true;
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
      "zroot/safe/home".use_template = ["home"];

      # Enable cleanup for synced backups
      "zroot/backups/current/desktop-caroline/data" = {
        use_template = ["data"];
        autosnap = false;
      };
      "zroot/backups/current/desktop-caroline/home" = {
        use_template = ["home"];
        autosnap = false;
      };
      "zroot/backups/current/desktop-elis/data" = {
        use_template = ["data"];
        autosnap = false;
      };
      "zroot/backups/current/desktop-elis/home" = {
        use_template = ["home"];
        autosnap = false;
      };
      "zroot/backups/current/desktop-elis/work-home" = {
        use_template = ["home"];
        autosnap = false;
      };
      "zroot/backups/current/laptop-private-caroline/data" = {
        use_template = ["data"];
        autosnap = false;
      };
      "zroot/backups/current/laptop-private-caroline/home" = {
        use_template = ["home"];
        autosnap = false;
      };
      "zroot/backups/current/laptop-private-elis/data" = {
        use_template = ["data"];
        autosnap = false;
      };
      "zroot/backups/current/laptop-private-elis/home" = {
        use_template = ["home"];
        autosnap = false;
      };
      "zroot/backups/current/laptop-work-elis/data" = {
        use_template = ["data"];
        autosnap = false;
      };
      "zroot/backups/current/laptop-work-elis/home" = {
        use_template = ["home"];
        autosnap = false;
      };
      "zroot/backups/current/vps06/data" = {
        use_template = ["data"];
        autosnap = false;
      };
      "zroot/backups/current/server-sparv/minecraft" = {
        use_template = ["data"];
        autosnap = false;
      };
      "zroot/backups/current/server-sparv/valheim-saves" = {
        use_template = ["data"];
        autosnap = false;
      };

      # Enable snapshotting for bulk storage
      "zstorage/files".use_template = ["storage"];
      "zstorage/files/audio".use_template = ["storage"];
      "zstorage/files/ebooks".use_template = ["storage"];
      "zstorage/files/software".use_template = ["storage"];
      "zstorage/files/video".use_template = ["storage"];
      "zstorage/files/video/movies".use_template = ["storage"];
      "zstorage/files/video/movies.sv".use_template = ["storage"];
      "zstorage/files/video/series".use_template = ["storage"];
      "zstorage/files/video/series.sv".use_template = ["storage"];
    };
    base.syncoid.enable = true;
    # Enable syncing of some filesystems
    base.syncoid.commands = {
      "root@desktop-caroline:zroot/safe/data".target = "zroot/backups/current/desktop-caroline/data";
      "root@desktop-caroline:zroot/safe/home".target = "zroot/backups/current/desktop-caroline/home";

      "root@desktop-elis:zroot/safe/data".target = "zroot/backups/current/desktop-elis/data";
      "root@desktop-elis:zroot/safe/home".target = "zroot/backups/current/desktop-elis/home";
      "root@desktop-elis:zroot/safe/work-home".target = "zroot/backups/current/desktop-elis/work-home";

      "root@laptop-private-caroline:zroot/safe/data".target = "zroot/backups/current/laptop-private-caroline/data";
      "root@laptop-private-caroline:zroot/safe/home".target = "zroot/backups/current/laptop-private-caroline/home";
      "root@laptop-private-caroline:zroot/zvol/win10".target = "zroot/backups/current/laptop-private-caroline/win10";

      "root@laptop-private-elis:zroot/safe/data".target = "zroot/backups/current/laptop-private-elis/data";
      "root@laptop-private-elis:zroot/safe/home".target = "zroot/backups/current/laptop-private-elis/home";

      "root@laptop-work-elis:zroot/safe/data".target = "zroot/backups/current/laptop-work-elis/data";
      "root@laptop-work-elis:zroot/safe/home".target = "zroot/backups/current/laptop-work-elis/home";

      "root@server-sparv:zroot/safe/valheim-saves".target = "zroot/backups/current/server-sparv/valheim-saves";
      "root@server-sparv:zroot/safe/minecraft".target = "zroot/backups/current/server-sparv/minecraft";

      "root@vps06:zroot/safe/data".target = "zroot/backups/current/vps06/data";
    };

    # Allow beszel to monitor this system
    services.beszel-agent.enable = true;
    services.beszel-agent.extraFilesystems = [
      "/boot"
      "/boot-fallback"
      "/data"
      "/data/home"
      "/data/local"
      "/media/zstorage"
      "/nix"
    ];

    # Enable monitoring hub of systems
    services.beszel-hub.enable = true;
    services.beszel-hub.settings = [
      {
        name = "server-main-elis";
        host = "server-main-elis";
        port = 45876;
      }
      {
        name = "vps06";
        host = "vps06";
        port = 45876;
      }
      {
        name = "server-sparv";
        host = "server-sparv";
        port = 45876;
      }
    ];
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # Hardware settings
  hardware.enableRedistributableFirmware = true;

  # Enable apcupsd.
  #services.apcupsd.enable = true;
  #services.apcupsd.configText = ''
  #  UPSTYPE usb
  #  NISIP 127.0.0.1
  #  BATTERYLEVEL 5 # Default from apcupsd: 5, NixOS default: 50
  #  MINUTES 1      # Default from apcupsd: 3, NixOS default: 5
  #'';

  # Enable smartd.
  services.smartd.enable = true;

  # Enable kvm
  virtualisation.libvirtd.enable = true;
  security.polkit.enable = true;

  # Set up Letsencrypt
  security.acme.defaults.email = config.etu.user.email;
  security.acme.acceptTerms = true;

  users.users.downloads = {
    group = "downloads";
    uid = 947;
    isSystemUser = true;
  };
  users.groups.downloads.gid = 947;

  # Install some additonal tools on this system.
  environment.systemPackages = [
    pkgs.mkvtoolnix-cli # mkvinfo and mkvmerge
    pkgs.ffmpeg # ffmpeg
  ];
}
