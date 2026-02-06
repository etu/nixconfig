{ config, ... }:
{
  programs.foot = {
    enable = true;
    settings = {
      main.term = "xterm-256color";
      main.font = "${config.etu.graphical.theme.fonts.monospace}:size=${builtins.toString config.etu.graphical.theme.fonts.size}";
      environment.TERMINAL = "foot";
    };
  };
}
