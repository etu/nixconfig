{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.graphical.fdm-printing.enable =
    lib.mkEnableOption "Enable graphical 3d printing settings";

  config = lib.mkIf config.etu.graphical.fdm-printing.enable {
    # Install using home-manager. FreeCAD (installed on NixOS) isn't
    # available for macOS in nixpkgs.
    home-manager.users.${config.etu.user.username}.home.packages = [
      pkgs.openscad
    ];
  };
}
