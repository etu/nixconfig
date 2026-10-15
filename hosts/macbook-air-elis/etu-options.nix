# Subset of the etu.* options from modules/nixos/ that shared home-manager
# modules in modules/home/ read through osConfig. Declared here so those
# modules can be imported unchanged on this nix-darwin host.
{ lib, ... }:
{
  options.etu = {
    dataPrefix = lib.mkOption {
      type = lib.types.str;
      default = "/data";
      description = "The path to where persistent storage happens";
    };
    user.username = lib.mkOption {
      type = lib.types.str;
      default = "elishirwing";
      description = "My username for this system.";
    };
    base.emacs.extraConfig = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "This allows adding strings that get added to the emacs config file.";
    };
    base.fish.enableUserZoxideCd = lib.mkEnableOption "Enable fish zoxide cd alias for normal users";
    base.fish.shellAbbrs = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [ ];
      description = "Shell abbreviations for fish";
    };
    # macOS needs the real font family name and renders point sizes smaller
    # than Linux, so these differ from the NixOS defaults.
    graphical.theme.fonts.monospace = lib.mkOption {
      type = lib.types.str;
      default = "JetBrains Mono";
      description = "Default monospace font to use";
    };
    graphical.theme.fonts.size = lib.mkOption {
      type = lib.types.int;
      default = 12;
      description = "Default font size";
    };
  };
}
