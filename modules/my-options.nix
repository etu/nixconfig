{ config, lib, pkgs, ... }:
{
  options.my = {
    spell.enable = lib.mkEnableOption "Enable and install aspell and hunspell with swedish and english dictionary";
  };
}
