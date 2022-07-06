{ config, lib, pkgs, ... }:
let
  cfg = config.my.home-manager;

  # Load sources
  sources = import ../nix/sources.nix;
in
{
  config = lib.mkIf cfg.enable {
    # Make sure to start the home-manager activation before I log it.
    systemd.services."home-manager-${config.etu.user.username}" = {
      before = [ "display-manager.service" ];
      wantedBy = [ "multi-user.target" ];
    };

    home-manager.users.${config.etu.user.username} = {
        # Import a persistance module for home-manager.
        imports = [
          ./home-manager/weechat.nix
        ];

        programs.home-manager.enable = true;

        home.file = {
          # Lorrirc
          ".direnvrc".text = ''
            use_nix() {
              eval "$(lorri direnv)"
            }
          '';
        };
      };
  };
}
