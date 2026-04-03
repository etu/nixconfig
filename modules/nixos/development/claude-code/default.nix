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
    # Enable unfree packages.
    etu.base.nix.allowUnfree = [
      "claude-code"
    ];

    # Install claude-code for my user.
    etu.user.extraUserPackages = [
      pkgs.claude-code
      pkgs.claude-monitor
    ];

    # Enable persistence for claude-code state files.
    etu.base.zfs.user.directories = [
      ".claude"
      ".claude-monitor"
    ];
  };
}
