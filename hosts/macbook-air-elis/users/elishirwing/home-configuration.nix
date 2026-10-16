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

  # The agent starts empty at login and only ssh itself adds keys on first
  # use, so git commit signing (modules/darwin/development/git) would prompt
  # for the passphrase after a reboot.
  # Load the keys from the Keychain into the agent at login instead.
  launchd.agents.ssh-add-keychain = {
    enable = true;
    config = {
      ProgramArguments = [
        "/usr/bin/ssh-add"
        "--apple-load-keychain"
      ];
      RunAtLoad = true;
    };
  };

  # Use the ssh-agent macOS already runs per login session (SSH_AUTH_SOCK is
  # set by launchd), with key passphrases stored in the Keychain. Store the
  # passphrase once with:
  # $ ssh-add --apple-use-keychain ~/.ssh/id_ed25519
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    settings."*" = {
      # Load keys into the agent on first use.
      AddKeysToAgent = "yes";

      # UseKeychain only exists in Apple's ssh (/usr/bin/ssh); don't let
      # other ssh builds fail on it.
      IgnoreUnknown = "UseKeychain";
      UseKeychain = "yes";
    };
  };

  # Used for backwards compatibility, read the release notes before changing.
  home.stateVersion = "26.11";
}
