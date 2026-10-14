{
  config,
  lib,
  ...
}:
{
  imports = [
    ./minecraft
  ];

  options.etu.games.enable = lib.mkEnableOption "Enable games settings";

  config = lib.mkIf config.etu.games.enable {
    etu.games = {
      minecraft.enable = lib.mkDefault true;
    };
  };
}
