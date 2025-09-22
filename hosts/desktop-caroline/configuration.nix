# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
#
# Initial deployment:
# 1. Put filesystem password in /tmp/secret.key
# 2. Deploy it from a remote system
#    $ nix run github:numtide/nixos-anywhere -- --flake .#desktop-caroline root@10.69.0.55
# 3. Reboot back into install system
# 4. sudo zpool import -f zroot
# 5. sudo zfs load-key -a -L prompt
# 6. sudo zfs change-key -o keylocation=prompt zroot
#
# Later deployments:
# $ deploy --skip-checks --targets .#laptop-private-caroline
{
  pkgs,
  myData,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix
  ];

  # Set hostname
  networking.hostName = "desktop-caroline";

  # Settings needed for ZFS
  networking.hostId = "56789235";

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
    user.extraGroups = ["video"];

    # Don't set a password for root / user depending on agenix.
    user.userPasswordAgeModule = myData.ageModules.hashed-caroline-laptop-concate-password;
    user.rootPasswordAgeModule = myData.ageModules.hashed-caroline-laptop-root-password;

    # Enable a graphical system.
    graphical.enable = true;
    graphical.sway.enable = true;
    graphical.sway.enableSuspendOnTimeout = false;
    graphical.fdm-printing.enable = true;

    # Do not enable Elis configs for certain things.
    base.emacs.enable = false;
    graphical.firefox.enable = false;
    graphical.gnupg.enable = false;
    graphical.flatpak.enablePersistence = true;
    theme.enable = true;

    games.enable = false;
    games.minecraft.enable = true;
    games.wowup.enable = true;
    games.steam.enable = true;

    # Install packages
    user.extraUserPackages = [
      # Other packages to have installed
      pkgs.firefox-bin
      pkgs.git
      pkgs.vscodium
      pkgs.evince
      pkgs.unzip
      pkgs.inkscape
      pkgs.libreoffice
      pkgs.blender
      pkgs.gnumake
      pkgs.nvtopPackages.amd
    ];

    # Allow home fileserver to connect to fetch snapshots.
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.syncoid.server-main-elis;

    # Allow to install some unfree packages.
    base.nix.allowUnfree = [
      "firefox-bin"
      "firefox-bin-unwrapped"
      "firefox-release-bin-unwrapped"
    ];

    # Configure sanoid snapshotting.
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
      "zroot/safe/home".use_template = ["home"];
    };
  };

  # Enable blueman.
  services.blueman.enable = true;
}
