{ config, lib, ... }:

{
  imports = [
    ./firefox
    ./sway
    ./telegram
    ./terminal
    ./theme
  ];

  options.etu.graphical.enable = lib.mkEnableOption "Enable graphical settings";

  config = lib.mkIf config.etu.graphical.enable {
    etu = {
      graphical.firefox.enable = true;
      graphical.sway.enable = true;
      graphical.telegram.enable = true;
      graphical.terminal.enable = true;
      graphical.theme.enable = true;
    };
  };
}
