{
  config,
  lib,
  ...
}:
{
  imports = [
    ./minecraft
    ./mumble
    ./steam
    ./steam-controller
    ./wowup
  ];

  options.etu.games.enable = lib.mkEnableOption "Enable games settings";

  config = lib.mkIf config.etu.games.enable {
    etu.games = {
      minecraft.enable = lib.mkDefault true;
      mumble.enable = lib.mkDefault true;
      steam.enable = lib.mkDefault true;
      steam-controller.enable = lib.mkDefault true;
    };
  };
}
