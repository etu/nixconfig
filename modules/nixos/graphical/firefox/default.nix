{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
{
  options.etu.graphical.firefox = {
    enable = lib.mkEnableOption "Enable graphical firefox settings";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.firefox-bin;
      description = "Firefox package to use.";
      readOnly = true;
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
  };
}
