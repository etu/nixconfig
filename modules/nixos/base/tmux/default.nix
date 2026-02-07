{
  config,
  lib,
  flake,
  ...
}:
{
  options.etu.base.tmux.enable = lib.mkEnableOption "Enable base tmux settings";

  config = lib.mkIf config.etu.base.tmux.enable {
    # Configure tmux for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.tmux
      ];
    };

    # Configure tmux for root users home-manager.
    home-manager.users.root.imports = [
      flake.homeModules.tmux
    ];
  };
}
