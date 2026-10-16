# home-manager configuration for elishirwing on macbook-air-elis.
#
# Picked up by blueprint and applied through nix-darwin's home-manager
# module, so it's activated with `just activate`.
#
# The shared home-manager modules (emacs, fish, ghostty, git, htop, tmux,
# vscode) and Claude Code are imported for this user by the darwin modules in
# modules/darwin/.
_: {
  # Let zsh (the macOS login shell) load the home-manager environment and
  # shell integrations below.
  programs.zsh.enable = true;

  # direnv with nix-direnv for cached `use flake` environments.
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  # The shared fish module already sets up the direnv hook itself.
  programs.direnv.enableFishIntegration = false;

  # Extra fish abbreviations for this host, merged with the ones from the
  # shared fish module.
  programs.fish.shellAbbrs = {
    l = "ls -lh";
  };

  # Used for backwards compatibility, read the release notes before changing.
  home.stateVersion = "26.11";
}
