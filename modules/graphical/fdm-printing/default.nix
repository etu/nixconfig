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
      pkgs.freecad-wayland
      pkgs.openscad-unstable
    ];

    # Enable persistence for fdm-printing related files.
    etu.base.zfs.user.directories = [
      ".config/FreeCAD"
      ".config/OpenSCAD"
    ];
  };
}
