# Git settings shared by NixOS and nix-darwin. Whether commits are signed by
# default is set by modules/nixos/development/git and
# modules/darwin/development/git.
{
  lib,
  osConfig,
  pkgs,
  ...
}:
{
  # Install the git helper scripts (git-branchclean, git-git, git-lol and
  # git-refetch-tags) into the profile, so they're in PATH on both NixOS and
  # macOS (~/bin is only in PATH on NixOS).
  home.packages = [
    (pkgs.runCommand "git-helper-scripts" { } ''
      install -Dm755 -t $out/bin ${./bin}/*
    '')
  ];

  # Allowed signers file for verifying SSH-signed git commits.
  home.file.".ssh/allowed_signers".text = lib.concatMapStrings (
    key: "${osConfig.etu.user.email} ${key}\n"
  ) (import ../../../pubkeys.nix).etu.computers;

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
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [
      ".AppleDB"
      ".AppleDesktop"
      ".AppleDouble"
      ".DocumentRevisions-V100"
      ".DS_Store"
      ".LSOverride"
      ".Spotlight-V100"
      ".TemporaryItems"
      ".Trashes"
      ".VolumeIcon.icns"
      "._*"
      ".apdisk"
      ".com.apple.timemachine.donotpresent"
      ".fseventsd"
      "Icon"
      "Network Trash Folder"
      "Temporary Items"
    ];
  };
}
