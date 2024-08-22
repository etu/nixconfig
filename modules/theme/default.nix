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
      programs.fish.catppuccin.enable = true;
      programs.rofi.catppuccin.enable = true;
      services.mako.catppuccin.enable = true;
      programs.swaylock.catppuccin.enable = true;

      # Bat module and theme
      programs.bat.enable = true;
      programs.bat.catppuccin.enable = true;

      # Fzf module and theme
      programs.fzf.enable = true;
      programs.fzf.catppuccin.enable = true;

      # Imv module and theme
      programs.imv.enable = true;
      programs.imv.catppuccin.enable = true;

      # Set up theme for sway.
      wayland.windowManager.sway.catppuccin.enable = true;
      wayland.windowManager.sway.config.colors = let
        background = "$base";
        focusedInactive = {
          background = "$base";
          border = "$overlay0";
          childBorder = "$overlay0";
          indicator = "$rosewater";
          text = "$text";
        };
        focused = {
          background = "$base";
          border = "$lavender";
          childBorder = "$lavender";
          indicator = "$rosewater";
          text = "$text";
        };
        urgent = {
          background = "$base";
          border = "$peach";
          childBorder = "$peach";
          indicator = "$overlay0";
          text = "$peach";
        };
        unfocused = focusedInactive;
        "placeholder" = focusedInactive;
      in {
        inherit background focused focusedInactive urgent unfocused "placeholder";
      };
    };
  };
}
