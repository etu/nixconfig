{ osConfig, ... }:
{
  # Configure alacritty
  programs.alacritty = {
    enable = true;
    settings = {
      env.TERMINAL = "alacritty";
      env.TERM = "xterm-256color";
      font.size = osConfig.etu.graphical.theme.fonts.size;
      font.normal.family = osConfig.etu.graphical.theme.fonts.monospace;
    };
  };
}
