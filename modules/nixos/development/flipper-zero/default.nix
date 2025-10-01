{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.development.flipper-zero.enable = lib.mkEnableOption "Enable flipper zero settings";

  config = lib.mkIf config.etu.development.flipper-zero.enable {
    # Install flipper zero udev rules.
    services.udev.packages = [
      pkgs.qFlipper
    ];

    # Install flipper zero program using home manager.
    etu.user.extraUserPackages = [ pkgs.qFlipper ];
  };
}
