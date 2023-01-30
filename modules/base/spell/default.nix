{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.base.spell.enable = lib.mkEnableOption "Enable base spell settings";

  config = lib.mkIf config.etu.base.spell.enable {
    environment.systemPackages = [
      pkgs.aspell

      # Dictionaries
      pkgs.aspellDicts.en
      pkgs.aspellDicts.en-computers
      pkgs.aspellDicts.en-science
      pkgs.aspellDicts.sv

      # Also install hunspell with dictionaries
      (pkgs.hunspellWithDicts [
        pkgs.hunspellDicts.en-gb-ise
        pkgs.hunspellDicts.en-gb-ize
        pkgs.hunspellDicts.en-us
        pkgs.hunspellDicts.sv-se
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
