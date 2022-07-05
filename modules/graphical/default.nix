{ config, lib, ... }:

{
  imports = [
    ./sway
    ./theme
  ];

  options.etu.graphical.enable = lib.mkEnableOption "Enable graphical settings";

  config = lib.mkIf config.etu.graphical.enable {
    etu = {
      graphical.sway.enable = true;
      graphical.theme.enable = true;
    };
  };
}
