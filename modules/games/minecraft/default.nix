{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.games.minecraft.enable = lib.mkEnableOption "Enable games minecraft settings";

  config = lib.mkIf config.etu.games.minecraft.enable {
    # Install minecraft using home manager.
    etu.user.extraUserPackages = [pkgs.prismlauncher];

    # Enable persistence for minecraft files.
    etu.base.zfs.user.directories = [
      ".local/share/PrismLauncher"
    ];
  };
}
