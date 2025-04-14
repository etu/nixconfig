{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.signal.enable = lib.mkEnableOption "Enable graphical signal settings";

  config = lib.mkIf config.etu.graphical.signal.enable {
    # Install signal desktop using home manager.
    etu.user.extraUserPackages = [pkgs.signal-desktop-bin];

    # Enable persistence for signal files.
    etu.base.zfs.user.directories = [
      ".config/Signal"
    ];
  };
}
