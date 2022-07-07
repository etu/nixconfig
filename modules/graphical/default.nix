{ config, lib, ... }:

{
  imports = [
    ./firefox
    ./gnupg
    ./spotify
    ./sway
    ./telegram
    ./terminal
    ./theme
    ./virtualbox
  ];

  options.etu.graphical.enable = lib.mkEnableOption "Enable graphical settings";

  config = lib.mkIf config.etu.graphical.enable {
    etu = {
      graphical.firefox.enable = true;
      graphical.gnupg.enable = true;
      graphical.sway.enable = true;
      graphical.telegram.enable = true;
      graphical.terminal.enable = true;
      graphical.theme.enable = true;
    };
  };
}
