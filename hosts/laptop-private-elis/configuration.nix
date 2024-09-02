# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
#
# Initial deployment:
# 1. Put filesystem password in /tmp/secret.key
# 2. Deploy it from a remote system
#    $ nix run github:numtide/nixos-anywhere -- --flake .#laptop-private-elis root@10.69.0.55
# 3. Reboot back into install system
# 4. sudo zpool import -f zroot
# 5. sudo zfs load-key -a -L prompt
# 6. sudo zfs change-key -o keylocation=prompt zroot
#
# Later deployments:
# $ deploy --skip-checks --targets .#laptop-private-elis
{
  config,
  lib,
  myData,
  pkgs,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix
  ];

  # Set hostname
  networking.hostName = "laptop-private-elis";

  # Settings needed for ZFS
  networking.hostId = "27416952";

  # My module settings
  etu = {
    stateVersion = "24.05";

    base.fish.enableUserZoxideCd = true;
    development.enable = true;
    development.flipper-zero.enable = true;
    games.enable = false;
    games.minecraft.enable = true;
    graphical.enable = true;
    graphical.sway.enable = true;
    graphical.fdm-printing.enable = true;
    graphical.hamradio.enable = true;
    graphical.signal.enable = true;
    graphical.flatpak.enablePersistence = true;
    graphical.flatpak.persistencePath = "${config.etu.dataPrefix}/flatpak-data";
    services.netdata.enable = true;
    theme.enable = true;
    user.enable = true;
    user.extraGroups = ["video" "docker" "libvirtd"];

    # Allow home fileserver to connect to fetch snapshots.
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.syncoid.server-main-elis;

    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
      "zroot/safe/home".use_template = ["home"];
    };
  };

  # Set up docker
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [virt-manager];
  virtualisation.spiceUSBRedirection.enable = true;

  # Enable blueman.
  services.blueman.enable = true;

  # Add community server to known hosts
  programs.ssh.knownHosts."aarch64.nixos.community".publicKey = myData.pubkeys.systems."aarch64.nixos.community";

  age.secrets = {
    inherit (myData.ageModules) "etu@aarch64.nixos.community" "etu@aarch64.nixos.community.pub";
    inherit (myData.ageModules) syncoid-workstations-ssh-ec;
  };

  # Set up remote builds
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "aarch64.nixos.community";
      maxJobs = 64;
      sshKey = config.age.secrets."etu@aarch64.nixos.community".path;
      sshUser = "etu";
      system = "aarch64-linux";
      supportedFeatures = ["big-parallel"];
    }
  ];

  # Specialisation for work while working with tickster projects
  specialisation.tickster.configuration = {
    nix.distributedBuilds = lib.mkForce false;

    etu = {
      # Turn off some of my default modules.
      development.flipper-zero.enable = lib.mkForce false;
      games.minecraft.enable = lib.mkForce false;
      graphical.fdm-printing.enable = lib.mkForce false;
      graphical.hamradio.enable = lib.mkForce false;
      graphical.signal.enable = lib.mkForce false;
      graphical.telegram.enable = lib.mkForce false;

      # Switch wallpaper
      graphical.sway.wallpaper = toString (pkgs.stdenv.mkDerivation {
        name = "tcab-bg";
        src = pkgs.fetchurl {
          url = "https://taserud.net/img/logo-dark.svg";
          sha256 = "sha256-PmWJ0pN+q6JyKOuQOHx0LFyGBFmGUZeKBoxLBO4Sn1E=";
        };
        dontUnpack = true;
        buildInputs = [pkgs.inkscape pkgs.graphicsmagick];
        installPhase = ''
          mkdir -p $out
          inkscape --export-type=png               \
                   --export-filename=logo-dark.png \
                   --export-dpi=400                \
                   $src

          gm convert -size 1920x1440 xc:#2d3640 bg.png
          gm composite -gravity center logo-dark.png bg.png $out/bg.png
        '';
      });

      # Switch lockscreen image
      graphical.sway.lockWallpaper = "${config.specialisation.tickster.configuration.etu.graphical.sway.wallpaper}/bg.png";

      # Enable snapshotting of filesystem while in use
      base.sanoid.datasets."zroot/safe/home-tickster".use_template = ["home"];

      # Disable snapshotting of my private home while work is in use
      base.sanoid.datasets."zroot/safe/home".use_template = lib.mkForce [];
    };

    # Run on a separate home directory for isolation and ease of backups
    fileSystems."${config.etu.dataPrefix}/home".device = lib.mkForce "zroot/safe/home-tickster";
  };
}
