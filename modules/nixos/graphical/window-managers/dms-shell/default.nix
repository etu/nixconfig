{
  config,
  lib,
  inputs,
  flake,
  ...
}:
{
  options.etu.graphical.window-managers.dms-shell.enable =
    lib.mkEnableOption "Enable DankMaterialShell, a Quickshell-based Material 3 desktop shell";

  config = lib.mkIf config.etu.graphical.window-managers.dms-shell.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        inputs.dms.homeModules.dank-material-shell
        flake.homeModules.dms-shell
      ];
    };
  };
}
