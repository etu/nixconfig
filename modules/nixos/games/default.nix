{
  config,
  lib,
  ...
}:
{
  options.etu.games.enable = lib.mkEnableOption "Enable games settings";

  config = lib.mkIf config.etu.games.enable { };
}
