{
  catppuccin,
  config,
  lib,
  ...
}: {
  options.etu.theme.enable = lib.mkEnableOption "Enable theme settings";
  options.etu.theme.flavor = lib.mkOption {
    type = lib.types.str;
    default = "mocha";
  };

  config = lib.mkIf config.etu.theme.enable {
    # Globally enable catppuccin themes on system level.
    catppuccin.enable = true;
    catppuccin.flavor = config.etu.theme.flavor;

    # Set up themes for home manager for the main user.
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Import the catppuccin home manager modules.
      imports = [
        catppuccin.homeManagerModules.catppuccin
      ];

      # Set Catppuccin flavor.
      catppuccin.flavor = config.etu.theme.flavor;

      # Enable catppuchin for on home manager level for different applications.
      programs.alacritty.catppuccin.enable = true;
    };
  };
}
