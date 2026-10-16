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
    # Install fish system wide, load the Nix environment in it and add it to
    # /etc/shells. nix-darwin only changes login shells of users listed in
    # users.knownUsers, so switch to it once with:
    # $ chsh -s /run/current-system/sw/bin/fish
    programs.fish.enable = true;
    environment.shells = [ pkgs.fish ];

    # Configure fish for my users home-manager.
    home-manager.users.${config.etu.user.username} = {
      imports = [
        flake.homeModules.fish
      ];

      # Extra abbreviations on macOS, merged with the ones from the shared
      # fish module.
      programs.fish.shellAbbrs = {
        l = "ls -lh";
      };
    };
  };
}
