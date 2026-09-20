{ osConfig, pkgs, ... }:
let
  # GTK 4.20+ no longer handles compose sequences (and dead keys) by itself on
  # Wayland unless an input method is running, which breaks the compose key.
  # Force GTK's built-in simple input method for Ghostty only.
  ghostty = pkgs.symlinkJoin {
    name = "ghostty-${pkgs.ghostty.version}";
    paths = [ pkgs.ghostty ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/ghostty --set GTK_IM_MODULE simple

      # The systemd unit and the D-Bus service file that ship with ghostty
      # (used when launching from a launcher) point at the unwrapped binary.
      # Point them at the wrapper instead.
      for file in share/dbus-1/services/com.mitchellh.ghostty.service \
                  share/systemd/user/app-com.mitchellh.ghostty.service; do
        rm $out/$file
        sed "s|${pkgs.ghostty}/bin/ghostty|$out/bin/ghostty|" \
            ${pkgs.ghostty}/$file > $out/$file
      done
    '';
    inherit (pkgs.ghostty) meta;
    passthru = { inherit (pkgs.ghostty) terminfo shell_integration vim; };
  };
in
{
  programs.ghostty = {
    enable = true;
    package = ghostty;

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
