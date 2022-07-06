{ config, lib, ... }:

{
  imports = [
    ./direnv
    ./git
  ];

  options.etu.development.enable = lib.mkEnableOption "Enable development settings";

  config = lib.mkIf config.etu.development.enable {
    etu = {
      development.direnv.enable = true;
      development.git.enable = true;
    };
  };
}
