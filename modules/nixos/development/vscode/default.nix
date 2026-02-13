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
    # Enable unfree packages on system level.
    etu.base.nix.allowUnfree = [
      "vscode-extension-bmewburn-vscode-intelephense-client"
      "vscode-extension-github-copilot"
    ];

    # Enable unfree packages on home level.
    etu.base.nix.allowUnfreeHome = [
      "vscode"
      "vscode-extension-bmewburn-vscode-intelephense-client"
      "vscode-extension-github-copilot"
    ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.vscode
      ];
    };

    # Enable persistence for vscode state files files.
    etu.base.zfs.user.directories = [
      ".config/Code/"
    ];
  };
}
