# Git settings shared by NixOS and nix-darwin. Whether commits are signed by
# default is set by modules/nixos/development/git and
# modules/darwin/development/git.
{ lib, osConfig, ... }:
{
  home.file = {
    "bin/git-branchclean".source = ./bin/git-branchclean;
    "bin/git-git".source = ./bin/git-git;
    "bin/git-lol".source = ./bin/git-lol;
    "bin/git-refetch-tags".source = ./bin/git-refetch-tags;

    # Allowed signers file for verifying SSH-signed git commits.
    ".ssh/allowed_signers".text = lib.concatMapStrings (
      key: "${osConfig.etu.user.email} ${key}\n"
    ) (import ../../../pubkeys.nix).etu.computers;
  };

  programs.git = {
    enable = true;

    # Default configs
    settings = {
      user.name = osConfig.etu.user.realname;
      user.email = osConfig.etu.user.email;
      user.signingKey = osConfig.etu.user.signingKey;

      # Use SSH keys for commit signing instead of GPG.
      gpg.format = "ssh";
      gpg.ssh.allowedSignersFile = "~/.ssh/allowed_signers";

      # Set default "git pull" behaviour so it doesn't try to default to
      # either "git fetch; git merge" (default) or "git fetch; git rebase".
      pull.ff = "only";

      # REuse REcorded REsolution to remember and resolve merge conflicts
      # better when you hit the several conflict several times.
      rerere.enabled = true;
    };

    # Global ignores
    ignores = [
      ".ac-php-conf.json"
      ".projectile-cache.eld"
    ];
  };
}
