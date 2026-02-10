{
  config,
  lib,
  ...
}:
{
  options.etu.base.nix.enable = lib.mkEnableOption "Enable base nix settings";
  options.etu.base.nix.allowUnfree = lib.mkOption {
    default = [ ];
    type = lib.types.listOf lib.types.str;
    description = "Enable unfree on system level";
  };
  options.etu.base.nix.allowUnfreeHome = lib.mkOption {
    default = [ ];
    type = lib.types.listOf lib.types.str;
    description = "Enable unfree on home level";
  };

  config = lib.mkIf config.etu.base.nix.enable {
    # If we allow certain unfree packages, enable the nix option to do so.
    nixpkgs.config.allowUnfreePredicate =
      (lib.mkIf ((builtins.length config.etu.base.nix.allowUnfree) > 0))
        (pkg: builtins.elem (lib.getName pkg) config.etu.base.nix.allowUnfree);

    # If we allow certain unfree packages on a home level, enable the nix option to do so.
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      nixpkgs.config.allowUnfreePredicate =
        (lib.mkIf ((builtins.length config.etu.base.nix.allowUnfreeHome) > 0))
          (pkg: builtins.elem (lib.getName pkg) config.etu.base.nix.allowUnfreeHome);
    };

    # Also configure unfree allowlist for root user's home-manager
    home-manager.users.root = {
      nixpkgs.config.allowUnfreePredicate =
        (lib.mkIf ((builtins.length config.etu.base.nix.allowUnfreeHome) > 0))
          (pkg: builtins.elem (lib.getName pkg) config.etu.base.nix.allowUnfreeHome);
    };

    # Extra binary caches
    nix.settings.substituters = [
      "https://nix-community.cachix.org"
      "https://etu.cachix.org"
    ];
    nix.settings.trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "etu.cachix.org-1:CeyfbMJZHZ95TScp8+I8+EeyzbncqPSj1xfCK9vOAFE="
    ];
  };
}
