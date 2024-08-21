{
  catppuccin,
  config,
  lib,
  ...
}: {
  options.etu.theme.enable = lib.mkEnableOption "Enable theme settings";

  config = lib.mkIf config.etu.theme.enable {
    # Globally enable catppuccin themes on system level.
    catppuccin.enable = true;

    # Set up themes for home manager for the main user.
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Import the catppuccin home manager modules.
      imports = [
        catppuccin.homeManagerModules.catppuccin
      ];
    };
  };
}
