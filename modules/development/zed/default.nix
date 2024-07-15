{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.development.zed.enable = lib.mkEnableOption "Enable zed editor settings";

  config = lib.mkIf config.etu.development.zed.enable {
    # Install zed editor program using home manager.
    etu.user.extraUserPackages = [pkgs.zed-editor];

    # Enable persistence for vscode state files files.
    etu.base.zfs.user.directories = [
      ".config/zed/"
      ".local/share/zed/"
    ];
  };
}
