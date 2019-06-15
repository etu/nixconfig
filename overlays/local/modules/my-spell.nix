{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.spell;

in {
  options.my.spell.enable = mkEnableOption "Enable and install aspell and hunspell with swedish and english dictionary";

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      aspell

      # Dictionaries
      aspellDicts.en aspellDicts.en-computers aspellDicts.en-science
      aspellDicts.sv

      # Also install hunspell with dictionaries
      (hunspellWithDicts [
        hunspellDicts.en-gb-ise
        hunspellDicts.en-gb-ize
        hunspellDicts.en-us
        hunspellDicts.sv-se
      ])
    ];

    # Configure aspell system wide
    environment.etc."aspell.conf".text = ''
      master en_US
      extra-dicts en-computers.rws
      add-extra-dicts en_US-science.rws sv.rws
    '';
  };
}
