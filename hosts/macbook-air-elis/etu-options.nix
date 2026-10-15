# Subset of the etu.* options from modules/nixos/ that shared home-manager
# modules in modules/home/ read through osConfig. Declared here so those
# modules can be imported unchanged on this nix-darwin host.
{ lib, ... }:
{
  options.etu = {
    user.username = lib.mkOption {
      type = lib.types.str;
      default = "elishirwing";
      description = "My username for this system.";
    };
    base.fish.enableUserZoxideCd = lib.mkEnableOption "Enable fish zoxide cd alias for normal users";
    base.fish.shellAbbrs = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [ ];
      description = "Shell abbreviations for fish";
    };
  };
}
