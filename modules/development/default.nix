{ config, lib, ... }:

{
  imports = [
    ./direnv
    ./flipper-zero
    ./git
    ./php
  ];

  options.etu.development.enable = lib.mkEnableOption "Enable development settings";

  config = lib.mkIf config.etu.development.enable {
    etu = {
      development.direnv.enable = true;
      development.git.enable = true;
      development.php.enable = true;

      # Define extra groups for user.
      user.extraGroups = [ "dialout" ];
    };
  };
}
