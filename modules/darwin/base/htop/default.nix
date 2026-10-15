{
  config,
  flake,
  lib,
  ...
}:
{
  options.etu.base.htop.enable = lib.mkEnableOption "Enable base htop settings";

  config = lib.mkIf config.etu.base.htop.enable {
    # Configure htop for my users home-manager.
    home-manager.users.${config.etu.user.username}.imports = [
      flake.homeModules.htop
    ];
  };
}
