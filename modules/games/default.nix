{ config, lib, ... }:

{
  imports = [
    ./minecraft
    ./mumble
    ./steam
  ];

  options.etu.games.enable = lib.mkEnableOption "Enable games settings";

  config = lib.mkIf config.etu.games.enable {
    etu.games = {
      minecraft.enable = true;
      mumble.enable = true;
      steam.enable = true;
    };
  };
}
