{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.games.mumble.enable = lib.mkEnableOption "Enable games mumble settings";

  config = lib.mkIf config.etu.games.mumble.enable {
    # Install mumble using home manager.
    etu.user.extraUserPackages = [pkgs.mumble];

    # Enable persistence for mumble files.
    etu.base.zfs.user.files = [
      ".config/Mumble/Mumble.conf"
      ".local/share/Mumble/Mumble/mumble.sqlite"
    ];
  };
}
