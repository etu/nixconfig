{
  config,
  lib,
  ...
}: {
  options.etu.base.tmux.enable = lib.mkEnableOption "Enable base tmux settings";

  config = lib.mkIf config.etu.base.htop.enable {
    # Install tmux
    programs.tmux.enable = true;
    programs.tmux.clock24 = true;

    # Configure tmux for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.file.".tmux.conf".source = ../../dotfiles/tmux.conf;
    };

    # Configure tmux for root users home-manager.
    home-manager.users.root.home.file.".tmux.conf".source = ../../dotfiles/tmux.conf;
  };
}
