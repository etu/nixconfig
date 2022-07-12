{ config, lib, pkgs, ... }:

{
  options.etu.games.minecraft.enable = lib.mkEnableOption "Enable games minecraft settings";

  config = lib.mkIf config.etu.games.minecraft.enable {
    # Allow to install minecraft.
    etu.base.nix.allowUnfree = [
      "minecraft-launcher"
    ];

    # Install mumble for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [ pkgs.minecraft ];
    };

    # Enable persistence for mumble files.
    environment.persistence.${config.etu.dataPrefix} = {
      users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
        directories = [
          ".minecraft"
        ];
      };
    };
  };
}
