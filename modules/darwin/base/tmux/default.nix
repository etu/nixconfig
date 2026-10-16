{
  config,
  flake,
  lib,
  ...
}:
{
  options.etu.base.tmux.enable = lib.mkEnableOption "Enable base tmux settings";

  config = lib.mkIf config.etu.base.tmux.enable {
    # Configure tmux for my users home-manager.
    home-manager.users.${config.etu.user.username} =
      { config, ... }:
      {
        imports = [
          flake.homeModules.tmux
        ];

        programs.tmux = {
          # Always start fish in new windows and panes. tmux otherwise uses
          # $SHELL, which on macOS is the account's login shell (zsh).
          shell = lib.getExe config.programs.fish.package;

          # The shared module reads the load average from /proc/loadavg, which
          # only exists on Linux. Set the status bar again with the macOS
          # equivalent, appended after the shared config so this one wins.
          extraConfig = ''
            set -g status-right '#[fg=colour2]][ #[fg=colour11]#(sysctl -n vm.loadavg | tr -d "{}" | xargs)#[fg=colour2] ][ #[fg=colour14]%Y-%m-%d %H:%M:%S#[fg=colour2] ]#[default]'
          '';
        };
      };
  };
}
