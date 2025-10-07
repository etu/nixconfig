{
  config,
  flake,
  lib,
  pkgs,
  ...
}:
{
  options.etu.base.htop.enable = lib.mkEnableOption "Enable base htop settings";

  config = lib.mkIf config.etu.base.htop.enable {
    # Always install htop as well as having it enabled in home-manager.
    environment.systemPackages = [ pkgs.htop ];

    # Configure htop for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.htop
      ];
    };

    # Configure htop for root users home-manager.
    home-manager.users.root.imports = [
      flake.homeModules.htop
    ];
  };
}
