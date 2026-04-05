{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.graphical.gnucash.enable = lib.mkEnableOption "Enable graphical gnucash settings";

  config = lib.mkIf config.etu.graphical.gnucash.enable {
    # Install gnucash using home-manager.
    etu.user.extraUserPackages = [
      pkgs.gnucash
    ];

    # Enable persistence for gnucash related files.
    etu.base.zfs.user.directories = [
      ".config/gnucash"
      ".local/share/gnucash"
    ];
  };
}
