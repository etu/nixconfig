{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.graphical.audacity.enable = lib.mkEnableOption "Enable graphical audacity settings";

  config = lib.mkIf config.etu.graphical.audacity.enable {
    # Install audacity using home-manager.
    etu.user.extraUserPackages = [
      pkgs.audacity
    ];

    # Enable persistence for audacity related files.
    etu.base.zfs.user.directories = [
      ".config/audacity"
    ];
  };
}
