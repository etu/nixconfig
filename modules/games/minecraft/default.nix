{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.games.minecraft.enable = lib.mkEnableOption "Enable games minecraft settings";

  config = lib.mkIf config.etu.games.minecraft.enable {
    # Allow to install minecraft.
    etu.base.nix.allowUnfree = [
      "minecraft-launcher"
    ];

    # Install minecraft using home manager.
    etu.user.extraUserPackages = [pkgs.minecraft];

    # Enable persistence for minecraft files.
    etu.base.zfs.user.directories = [
      ".minecraft"
    ];
  };
}
