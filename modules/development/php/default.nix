{ config, lib, pkgs, ... }:

{
  options.etu.development.php.enable = lib.mkEnableOption "Enable development php settings";

  config = lib.mkIf config.etu.development.enable {
    # Install php utils using home manager.
    etu.user.extraUserPackages = [
      # PHP utils
      pkgs.php
      pkgs.php.packages.composer
      pkgs.php.packages.phpcbf
      pkgs.php.packages.phpcs
    ];
  };
}
