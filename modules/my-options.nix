{ config, lib, pkgs, ... }:
{
  options.my = {
    backup = {
      enable = lib.mkEnableOption "Enables backup related thingys";
      enableSanoid = lib.mkEnableOption "Enables snapshot creation";
      enableSyncoid = lib.mkEnableOption "Enables snapshot syncing";
    };
    spell.enable = lib.mkEnableOption "Enable and install aspell and hunspell with swedish and english dictionary";
  };
}
