# home-manager configuration for elishirwing on macbook-air-elis.
#
# Picked up by blueprint and applied through nix-darwin's home-manager
# module, so it's activated with `just switch-macbook-air-elis`.
{ flake, ... }:
{
  # Shared home-manager modules from modules/home/ that don't depend on
  # NixOS-only osConfig.etu.* options.
  imports = [
    flake.homeModules.htop
  ];

  # Let zsh (the macOS login shell) load the home-manager environment and
  # shell integrations below.
  programs.zsh.enable = true;

  # direnv with nix-direnv for cached `use flake` environments.
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  # Git settings, mirroring modules/nixos/development/git on the NixOS hosts.
  programs.git = {
    enable = true;

    settings = {
      user.name = "Elis Hirwing";
      user.email = "elis@hirwing.se";

      # Set default "git pull" behaviour so it doesn't try to default to
      # either "git fetch; git merge" (default) or "git fetch; git rebase".
      pull.ff = "only";

      # REuse REcorded REsolution to remember and resolve merge conflicts
      # better when you hit the several conflict several times.
      rerere.enabled = true;
    };
  };

  # Used for backwards compatibility, read the release notes before changing.
  home.stateVersion = "26.11";
}
