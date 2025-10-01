{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.base.spell.enable = lib.mkEnableOption "Enable base spell settings";

  config = lib.mkIf config.etu.base.spell.enable {
    etu.user.extraUserPackages = [
      # Aspell Dictionaries
      (pkgs.aspellWithDicts (dicts: [
        dicts.en
        dicts.en-computers
        dicts.en-science
        dicts.sv
      ]))

      # Hunspell Dictionaries
      (pkgs.hunspell.withDicts (dicts: [
        dicts.en_GB-ize
        dicts.en_US
        dicts.sv_SE
      ]))
    ];

    etu.base.nix.allowUnfree = [
      "aspell-dict-en-science"
    ];
  };
}
