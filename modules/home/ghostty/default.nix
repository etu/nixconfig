{ osConfig, perSystem, ... }:
{
  # Since Ghostty 1.3 the GTK setting for primary paste is honored, and it
  # defaults to false outside of GNOME, which disables middle-click paste.
  dconf.settings."org/gnome/desktop/interface".gtk-enable-primary-paste = true;

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

      # Don't start new windows in the directory of the previous terminal, they
      # should start in the home directory (tabs and splits still inherit).
      window-inherit-working-directory = false;

      # Always use a solid block cursor. Shell integration would otherwise
      # switch the cursor to a bar at the prompt.
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration-features = "no-cursor";

      # Selecting text should copy it (middle-click paste is enabled via dconf above).
      copy-on-select = true;

      # Don't let Ctrl+Enter toggle fullscreen by default.
      keybind = [ "ctrl+enter=unbind" ];
    };
  };
}
