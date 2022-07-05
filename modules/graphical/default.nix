{ config, lib, ... }:

{
  imports = [
    ./theme
  ];

  options.etu.graphical.enable = lib.mkEnableOption "Enable graphical settings";

  config = lib.mkIf config.etu.graphical.enable {
    etu = {
      graphical.theme.enable = true;
    };
  };
}
