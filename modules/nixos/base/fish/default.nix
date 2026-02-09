{
  config,
  flake,
  lib,
  pkgs,
  ...
}:
{
  options.etu.base.fish.enable = lib.mkEnableOption "Enable base fish settings";
  options.etu.base.fish.enableUserZoxideCd =
    lib.mkEnableOption "Enable fish zoxide cd alias for normal users";
  options.etu.base.fish.shellAbbrs = lib.mkOption {
    type = lib.types.listOf lib.types.attrs;
    default = [ ];
    example = [
      {
        name = "..";
        value = "cd ..";
      }
    ];
    description = "Shell abbreviations for fish";
  };

  config = lib.mkIf config.etu.base.fish.enable {
    # Enable fish.
    programs.fish.enable = true;

    # My main users shell.
    users.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      shell = pkgs.fish;
    };

    # Root shell.
    users.users.root.shell = pkgs.fish;

    # Configure fish for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.fish
      ];
    };

    # Configure fish for root users home-manager.
    home-manager.users.root.imports = [
      flake.homeModules.fish
    ];

    # Enable persistence for fish files.
    etu.base.zfs.user.directories = [
      ".config/fish"
      ".local/share/fish"
      ".local/share/zoxide"
    ];
  };
}
