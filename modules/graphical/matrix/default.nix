{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.matrix.enable = lib.mkEnableOption "Enable graphical matrix settings";

  config = lib.mkIf config.etu.graphical.matrix.enable {
    # Install the telegram chat client using home manager.
    etu.user.extraUserPackages = [
      pkgs.element-desktop-wayland
    ];

    # Enable persistence for telegram files.
    etu.base.zfs.user.directories = [
      ".config/Element"
    ];
  };
}
