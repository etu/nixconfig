{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.fdm-printing.enable = lib.mkEnableOption "Enable graphical 3d printing settings";

  config = lib.mkIf config.etu.graphical.fdm-printing.enable {
    # Install using home-manager.
    etu.user.extraUserPackages = [
      pkgs.cura
      pkgs.openscad
      pkgs.prusa-slicer
    ];

    # Enable persistence for fdm-printing related files.
    etu.base.zfs.user.directories = [
      ".cache/cura"
      ".config/cura"
      ".local/share/cura"
      ".config/OpenSCAD"
      ".config/PrusaSlicer"
    ];
  };
}
