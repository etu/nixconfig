{ osConfig, ... }:
{
  programs.foot = {
    enable = true;
    settings = {
      main.term = "xterm-256color";
      main.font = "${osConfig.etu.graphical.theme.fonts.monospace}:size=${builtins.toString osConfig.etu.graphical.theme.fonts.size}";
      environment.TERMINAL = "foot";
    };
  };
}
