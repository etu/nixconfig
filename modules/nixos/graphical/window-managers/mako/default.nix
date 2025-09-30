{
  config,
  lib,
  ...
}: {
  options.etu.graphical.window-managers.mako.enable = lib.mkEnableOption "Enable mako, a notification daemon for wayland";

  config = lib.mkIf config.etu.graphical.window-managers.mako.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Set up mako, a notification deamon for wayland
      services.mako = {
        enable = true;
        settings = {
          border-size = 3;
          default-timeout = 6000;
          font = "${config.etu.graphical.theme.fonts.monospace} ${toString config.etu.graphical.theme.fonts.size}";
        };
      }; # END mako
    };
  };
}
