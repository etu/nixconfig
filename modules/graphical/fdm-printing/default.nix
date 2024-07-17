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
      pkgs.openscad-unstable
      pkgs.prusa-slicer
      # pkgs.super-slicer-latest
    ];

    # Enable persistence for fdm-printing related files.
    etu.base.zfs.user.directories = [
      ".config/OpenSCAD"
      ".config/PrusaSlicer"
      ".config/SuperSlicer"
    ];
  };
}
