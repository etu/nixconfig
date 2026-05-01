{
  config,
  pkgs,
  lib,
  flake,
  ...
}:
let
  # Define Firefox extensions with NUR packages
  defaultExtensions = [
    pkgs.nur.repos.rycee.firefox-addons.elasticvue
    pkgs.nur.repos.rycee.firefox-addons.facebook-container
    pkgs.nur.repos.rycee.firefox-addons.firefox-color
    pkgs.nur.repos.rycee.firefox-addons.multi-account-containers
    pkgs.nur.repos.rycee.firefox-addons.privacy-badger
    pkgs.nur.repos.rycee.firefox-addons.proton-pass
    pkgs.nur.repos.rycee.firefox-addons.sponsorblock
    pkgs.nur.repos.rycee.firefox-addons.streetpass-for-mastodon
    pkgs.nur.repos.rycee.firefox-addons.swedish-dictionary
    pkgs.nur.repos.rycee.firefox-addons.terms-of-service-didnt-read
    pkgs.nur.repos.rycee.firefox-addons.ublock-origin
  ];
in
{
  options.etu.graphical.firefox = {
    enable = lib.mkEnableOption "Enable graphical firefox settings";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.firefox-bin;
      description = "Firefox package to use.";
      readOnly = true;
    };
    extensions = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = defaultExtensions;
      description = "Firefox extensions to install.";
    };
  };

  config = lib.mkIf config.etu.graphical.firefox.enable {
    # Allow to install some unfree packages.
    etu.base.nix.allowUnfree = [
      "firefox-bin"
      "firefox-bin-unwrapped"
      "firefox-release-bin-unwrapped"
    ];

    # Allow to install some unfree packages.
    etu.base.nix.allowUnfreeHome = [
      "firefox-bin"
    ];

    # Configure firefox for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.firefox
      ];
    };

    # Enable persistence for firefox files.
    etu.base.zfs.user.directories = [
      ".mozilla/firefox/default"
    ];
    etu.base.zfs.user.files = [
      ".mozilla/firefox/profiles.ini"
    ];
  };
}
