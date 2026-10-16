{
  config,
  lib,
  flake,
  perSystem,
  ...
}:
{
  options.etu.graphical.ghostty.enable = lib.mkEnableOption "Enable ghostty terminal emulator";

  config = lib.mkIf config.etu.graphical.ghostty.enable {
    # If my user exists, enable the home-manager configuration for ghostty.
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.ghostty
      ];

      # Since Ghostty 1.3 the GTK setting for primary paste is honored, and it
      # defaults to false outside of GNOME, which disables middle-click paste.
      dconf.settings."org/gnome/desktop/interface".gtk-enable-primary-paste = true;

      programs.ghostty = {
        package = perSystem.self.ghostty;

        # The unit shipped in the (wrapped) package is already picked up
        # through the profile, so don't let home-manager install another copy
        # of it.
        systemd.enable = false;

        settings = {
          term = "xterm-256color";
          env = [ "TERMINAL=ghostty" ];

          # Don't let Ctrl+Enter toggle fullscreen by default.
          keybind = [ "ctrl+enter=unbind" ];
        };
      };
    };
  };
}
