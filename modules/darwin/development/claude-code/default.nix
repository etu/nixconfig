{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.development.claude-code.enable =
    lib.mkEnableOption "Enable development claude-code settings";

  config = lib.mkIf config.etu.development.claude-code.enable {
    etu.base.nix.allowUnfree = [
      "claude-code"
    ];

    home-manager.users.${config.etu.user.username}.home.packages = [
      pkgs.claude-code
      pkgs.claude-monitor
    ];
  };
}
