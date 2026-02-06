{ config, ... }:
{
  # Configure alacritty
  programs.alacritty = {
    enable = true;
    settings = {
      env.TERMINAL = config.etu.graphical.terminal.terminalName;
      env.TERM = "xterm-256color";
      font.size = config.etu.graphical.theme.fonts.size;
      font.normal.family = config.etu.graphical.theme.fonts.monospace;
    };
  };
}
