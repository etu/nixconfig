{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.games.wowup.enable = lib.mkEnableOption "Enable games wowup settings";

  config = lib.mkIf config.etu.games.wowup.enable {
    # Allow to install some unfree packages.
    etu.base.nix.allowUnfree = [
      "wowup-cf"
    ];
    # Install steam using home manager.
    etu.user.extraUserPackages = [pkgs.wowup-cf];

    # Enable persistence for steam files.
    etu.base.zfs.user.directories = [
      ".config/WowUpCf"
    ];
  };
}
