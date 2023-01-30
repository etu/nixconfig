{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.spotify.enable = lib.mkEnableOption "Enable graphical spotify settings";

  config = lib.mkIf config.etu.graphical.spotify.enable {
    # Allow to install spotify.
    etu.base.nix.allowUnfree = [
      "spotify"
      "spotify-unwrapped"
    ];

    # Install spotify using home manager.
    etu.user.extraUserPackages = [pkgs.spotify];

    # Enable persistence for spotify files.
    etu.base.zfs.user.files = [
      ".config/spotify/prefs"
    ];
    etu.base.zfs.user.directories = [
      ".config/spotify/Users"
    ];
  };
}
