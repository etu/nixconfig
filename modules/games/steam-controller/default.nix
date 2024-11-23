{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.games.steam-controller.enable = lib.mkEnableOption "Enable games steam-controller settings";

  config = lib.mkIf config.etu.games.steam-controller.enable {
    # Enable udev rules for steam controller
    services.udev.packages = [
      pkgs.sc-controller
    ];

    # Install steam using home manager.
    etu.user.extraUserPackages = [pkgs.sc-controller];
  };
}
