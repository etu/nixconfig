# home-manager configuration for elishirwing on macbook-air-elis.
#
# Picked up by blueprint and applied through nix-darwin's home-manager
# module, so it's activated with `just switch-macbook-air-elis`.
{
  config,
  flake,
  lib,
  pkgs,
  ...
}:
{
  # Shared home-manager modules from modules/home/. The emacs and fish modules
  # read a few osConfig.etu.* options, which ../../etu-options.nix declares
  # for this host.
  imports = [
    flake.homeModules.emacs
    flake.homeModules.fish
    flake.homeModules.htop
  ];

  # Let zsh (the macOS login shell) load the home-manager environment and
  # shell integrations below.
  programs.zsh.enable = true;

  # direnv with nix-direnv for cached `use flake` environments.
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  # The shared fish module already sets up the direnv hook itself.
  programs.direnv.enableFishIntegration = false;

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

  # Ghostty, with the macOS build of the package and the platform-neutral
  # settings from modules/home/ghostty (which is Linux-specific).
  programs.ghostty = {
    enable = true;
    package = pkgs.ghostty-bin;

    settings = {
      # Always start fish, regardless of the account's login shell.
      command = lib.getExe config.programs.fish.package;

      # Don't start new windows in the directory of the previous terminal, they
      # should start in the home directory (tabs and splits still inherit).
      window-inherit-working-directory = false;

      # Always use a solid block cursor. Shell integration would otherwise
      # switch the cursor to a bar at the prompt.
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration-features = "no-cursor";

      # Selecting text should copy it.
      copy-on-select = true;
    };
  };

  # Claude Code, like modules/nixos/development/claude-code on the NixOS hosts.
  home.packages = [
    pkgs.claude-code
    pkgs.claude-monitor
  ];

  # Used for backwards compatibility, read the release notes before changing.
  home.stateVersion = "26.11";
}
