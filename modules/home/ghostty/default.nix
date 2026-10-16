# Ghostty settings shared by NixOS and nix-darwin. The platform specific parts
# (package, Linux desktop integration, macOS app behaviour) are set by
# modules/nixos/graphical/ghostty and modules/darwin/graphical/ghostty.
{ osConfig, ... }:
{
  programs.ghostty = {
    enable = true;

    settings = {
      font-family = osConfig.etu.graphical.theme.fonts.monospace;
      font-size = osConfig.etu.graphical.theme.fonts.size;

      # Don't start new windows in the directory of the previous terminal, they
      # should start in the home directory (tabs and splits still inherit).
      window-inherit-working-directory = false;

      # Always use a solid block cursor. Shell integration would otherwise
      # switch the cursor to a bar at the prompt (no-cursor). Over ssh, install
      # the xterm-ghostty terminfo on remote hosts (ssh-terminfo), and fall back
      # to TERM=xterm-256color when that isn't possible (ssh-env).
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration-features = "no-cursor,ssh-terminfo,ssh-env";

      # Selecting text should copy it.
      copy-on-select = true;
    };
  };
}
