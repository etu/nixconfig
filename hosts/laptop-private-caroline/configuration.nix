# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
#
# Initial deployment:
# 1. Put filesystem password in /tmp/secret.key
# 2. Deploy it from a remote system
#    $ nix run github:numtide/nixos-anywhere -- --flake .#laptop-private-caroline root@10.69.0.55
# 3. Reboot back into install system
# 4. sudo zpool import -f zroot
# 5. sudo zfs load-key -a -L prompt
# 6. sudo zfs change-key -o keylocation=prompt zroot
#
# Later deployments:
# $ deploy --skip-checks --targets .#laptop-private-caroline
{
  config,
  pkgs,
  myData,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix
  ];

  # Set hostname
  networking.hostName = "laptop-private-caroline";

  # Settings needed for ZFS
  networking.hostId = "36522517";

  # My module settings
  etu = {
    stateVersion = "24.05";

    # This is to make the openssh identity files to be located in a
    # reasonable place.
    dataPrefix = "/data";

    # Enable my user account.
    user.enable = true;
    user.username = "concate";
    user.realname = "Caroline Hirwing";
    user.email = "caroline@hirwing.se";
    user.extraGroups = ["docker" "video" "libvirtd"];

    # Don't set a password for root / user depending on agenix.
    user.userPasswordAgeModule = myData.ageModules.hashed-caroline-laptop-concate-password;
    user.rootPasswordAgeModule = myData.ageModules.hashed-caroline-laptop-root-password;

    # Enable a graphical system.
    graphical.enable = true;
    graphical.sway.enable = true;
    graphical.fdm-printing.enable = true;

    # Do not enable Elis configs for certain things.
    base.emacs.enable = false;
    graphical.firefox.enable = false;
    graphical.gnupg.enable = false;
    graphical.telegram.enable = false;
    graphical.flatpak.enablePersistence = true;
    graphical.flatpak.persistencePath = "${config.etu.dataPrefix}/flatpak-data";
    theme.enable = true;

    games.minecraft.enable = true;

    # Install packages
    user.extraUserPackages = [
      pkgs.firefox
      pkgs.git
      pkgs.vscodium
      pkgs.virt-manager
      pkgs.evince
      pkgs.unzip
      pkgs.inkscape
      pkgs.libreoffice
      pkgs.blender
      pkgs.minikube

      (pkgs.rWrapper.override {
        packages = with pkgs.rPackages; [
          FactoMineR
          GGally
          ISLR
          MASS
          arsenal
          broom
          caTools
          coefplot
          corrplot
          cowplot
          devtools
          esquisse
          gapminder
          ggiraph
          ggrepel
          ggridges
          gridExtra
          here
          interplot
          mapdata
          mapproj
          maps
          margins
          patchwork
          quantreg
          rlang
          scales
          srvyr
          survey
          tidyverse
          tinytex
          viridis
          viridisLite
        ];
      })

      (pkgs.rstudioWrapper.override {
        packages = with pkgs.rPackages; [
          FactoMineR
          GGally
          ISLR
          MASS
          arsenal
          broom
          caTools
          coefplot
          corrplot
          cowplot
          devtools
          esquisse
          gapminder
          ggiraph
          ggrepel
          ggridges
          gridExtra
          here
          interplot
          mapdata
          mapproj
          maps
          margins
          patchwork
          quantreg
          rlang
          scales
          srvyr
          survey
          tidyverse
          tinytex
          viridis
          viridisLite
        ];
      })
    ];

    # Allow home fileserver to connect to fetch snapshots.
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.syncoid.server-main-elis;

    # Configure sanoid snapshotting.
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
      "zroot/safe/home".use_template = ["home"];
      "zroot/zvol/win10".use_template = ["data"];
    };
  };

  # Enable blueman.
  services.blueman.enable = true;

  # Enable docker deamon
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up virt-managar
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
}
