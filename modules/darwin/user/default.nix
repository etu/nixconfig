{
  config,
  lib,
  perSystem,
  ...
}:
{
  options.etu = {
    stateVersion = lib.mkOption {
      type = lib.types.str;
      example = "26.11";
      description = "The home-manager state version to use for my user (system.stateVersion is set separately on nix-darwin).";
    };

    user = {
      username = lib.mkOption {
        type = lib.types.str;
        default = "etu";
        description = "My username for this system.";
      };
      realname = lib.mkOption {
        type = lib.types.str;
        default = "Elis Hirwing";
        description = "My realname for this system.";
      };
      email = lib.mkOption {
        type = lib.types.str;
        default = "elis@hirwing.se";
        description = "My email for this system.";
      };
      signingKey = lib.mkOption {
        type = lib.types.str;
        default = "~/.ssh/id_ed25519.pub";
        description = "My public signing key for this system.";
      };
    };
  };

  config = {
    # User that user-specific options (defaults, homebrew, etc) apply to.
    system.primaryUser = config.etu.user.username;
    users.users.${config.etu.user.username}.home = "/Users/${config.etu.user.username}";

    # Reuse the system's pkgs (with its overlays, e.g. NUR) instead of having
    # home-manager instantiate its own separate copy, and install user packages
    # into /etc/profiles/per-user (which e.g. the emacs module puts on PATH).
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;

    # Pass perSystem to home-manager modules.
    home-manager.extraSpecialArgs = { inherit perSystem; };

    home-manager.users.${config.etu.user.username} = {
      # Let zsh (the macOS login shell) load the home-manager environment and
      # shell integrations (such as direnv's hook).
      programs.zsh.enable = true;

      # Used for backwards compatibility, read the release notes before changing.
      home.stateVersion = config.etu.stateVersion;
    };
  };
}
