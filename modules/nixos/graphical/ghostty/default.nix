{
  config,
  lib,
  flake,
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
    };
  };
}
