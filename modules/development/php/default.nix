{ config, lib, pkgs, ... }:

{
  options.etu.development.php.enable = lib.mkEnableOption "Enable development php settings";

  config = lib.mkIf config.etu.development.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [
        # PHP utils
        pkgs.php
        pkgs.php.packages.composer
        pkgs.php.packages.phpcbf
        pkgs.php.packages.phpcs
      ];
    };
  };
}
