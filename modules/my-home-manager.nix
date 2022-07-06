{ config, lib, pkgs, ... }:
let
  cfg = config.my.home-manager;

  # Load sources
  sources = import ../nix/sources.nix;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${config.etu.user.username} = {
        # Import a persistance module for home-manager.
        imports = [
          ./home-manager/weechat.nix
        ];

        programs.home-manager.enable = true;
      };
  };
}
