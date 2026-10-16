# home-manager configuration for elishirwing on macbook-air-elis.
#
# Picked up by blueprint and applied through nix-darwin's home-manager
# module, so it's activated with `just activate`.
#
# The shared home-manager modules (emacs, fish, htop) and Claude Code are
# imported for this user by the darwin modules in modules/darwin/.
{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:
{
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

  # Git settings, mirroring modules/nixos/development/git on the NixOS hosts.
  programs.git = {
    enable = true;

    settings = {
      user.name = "Elis Hirwing";
      user.email = "elis@hirwing.se";
      user.signingKey = "~/.ssh/id_ed25519.pub";

      # Sign commits with the SSH key (through the ssh-agent) instead of GPG.
      commit.gpgSign = true;
      gpg.format = "ssh";
      gpg.ssh.allowedSignersFile = "~/.ssh/allowed_signers";

      # Set default "git pull" behaviour so it doesn't try to default to
      # either "git fetch; git merge" (default) or "git fetch; git rebase".
      pull.ff = "only";

      # REuse REcorded REsolution to remember and resolve merge conflicts
      # better when you hit the several conflict several times.
      rerere.enabled = true;
    };
  };

  # Allowed signers file for verifying SSH-signed git commits.
  home.file.".ssh/allowed_signers".text = lib.concatMapStrings (
    key: "elis@hirwing.se ${key}\n"
  ) (import ../../../../pubkeys.nix).etu.computers;

  # The agent starts empty at login and only ssh itself adds keys on first
  # use, so commit signing would prompt for the passphrase after a reboot.
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

  # Ghostty, with the macOS build of the package and the platform-neutral
  # settings from modules/home/ghostty (which is Linux-specific).
  programs.ghostty = {
    enable = true;
    package = pkgs.ghostty-bin;

    settings = {
      # Always start fish, regardless of the account's login shell.
      command = lib.getExe config.programs.fish.package;

      # Same font as Emacs, from etu.graphical.theme.fonts (modules/darwin).
      font-family = osConfig.etu.graphical.theme.fonts.monospace;
      font-size = osConfig.etu.graphical.theme.fonts.size;

      # Don't start new windows in the directory of the previous terminal, they
      # should start in the home directory (tabs and splits still inherit).
      window-inherit-working-directory = false;

      # Always use a solid block cursor. Shell integration would otherwise
      # switch the cursor to a bar at the prompt (no-cursor). Over ssh, install
      # the xterm-ghostty terminfo on remote hosts (ssh-terminfo), and fall back
      # to TERM=xterm-256color when that isn't possible (ssh-env).
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration-features = "no-cursor,ssh-terminfo,ssh-env";

      # Selecting text should copy it.
      copy-on-select = true;

      # Quit when the last window is closed, like on Linux. Otherwise every
      # instance started by AeroSpace's Alt+Return (open -na Ghostty) stays
      # running in the Dock after its window is closed.
      quit-after-last-window-closed = true;
    };
  };

  # Used for backwards compatibility, read the release notes before changing.
  home.stateVersion = "26.11";
}
