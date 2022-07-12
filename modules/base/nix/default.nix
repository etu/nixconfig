{ config, lib, ... }:

{
  options.etu.base.nix.enable = lib.mkEnableOption "Enable base nix settings";
  options.etu.base.nix.allowUnfree = lib.mkOption {
    default = [ ];
    type = lib.types.listOf lib.types.str;
    description = "Enable base nix settings";
  };

  config = lib.mkIf config.etu.base.nix.enable {
    # If we allow certain unfree packages, enable the nix option to do so.
    nixpkgs.config.allowUnfreePredicate = (lib.mkIf (
      (builtins.length config.etu.base.nix.allowUnfree) > 0
    )) (pkg: builtins.elem (lib.getName pkg) config.etu.base.nix.allowUnfree);
  };
}
