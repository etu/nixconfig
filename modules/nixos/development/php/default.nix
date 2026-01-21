{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.development.php.enable = lib.mkEnableOption "Enable development php settings";

  config = lib.mkIf config.etu.development.enable {
    # Enable persistence for composer files.
    etu.base.zfs.user.directories = [
      ".config/composer"
    ];

    # Install php utils using home manager.
    etu.user.extraUserPackages = [
      # PHP utils
      pkgs.php
      pkgs.php.packages.composer
      pkgs.php.packages.php-codesniffer
    ];
  };
}
