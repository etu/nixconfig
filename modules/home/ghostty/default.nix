{ osConfig, perSystem, ... }:
{
  programs.ghostty = {
    enable = true;
    package = perSystem.self.ghostty;

    # The unit shipped in the (wrapped) package is already picked up through the
    # profile, so don't let home-manager install another copy of it.
    systemd.enable = false;

    settings = {
      term = "xterm-256color";
      env = [ "TERMINAL=ghostty" ];
      font-family = osConfig.etu.graphical.theme.fonts.monospace;
      font-size = osConfig.etu.graphical.theme.fonts.size;

      # Always use a solid block cursor. Shell integration would otherwise
      # switch the cursor to a bar at the prompt.
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration-features = "no-cursor";
    };
  };
}
