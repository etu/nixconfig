{
  config,
  flake,
  lib,
  ...
}:
{
  options.etu.development.vscode.enable = lib.mkEnableOption "Enable development vscode settings";
  options.etu.development.vscode.enableWork =
    lib.mkEnableOption "Enable development vscode for work settings";

  config = lib.mkIf config.etu.development.vscode.enable {
    # Enable unfree packages.
    etu.base.nix.allowUnfree = [
      "vscode"
      "vscode-extension-bmewburn-vscode-intelephense-client"
      "vscode-extension-github-copilot"
    ];

    # Configure vscode for my users home-manager.
    home-manager.users.${config.etu.user.username}.imports = [
      flake.homeModules.vscode
    ];
  };
}
