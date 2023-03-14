{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.cura.enable = lib.mkEnableOption "Enable graphical evolution settings";

  config = lib.mkIf config.etu.graphical.cura.enable {
    # Install using home-manager.
    etu.user.extraUserPackages = [pkgs.cura];

    # Enable persistence for evolution files.
    etu.base.zfs.user.directories = [
      ".cache/cura/"
      ".config/cura"
      ".local/share/cura"
    ];
  };
}
