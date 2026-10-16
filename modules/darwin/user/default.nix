{
  config,
  lib,
  ...
}:
{
  options.etu.user = {
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

  config = {
    # User that user-specific options (defaults, homebrew, etc) apply to.
    system.primaryUser = config.etu.user.username;
    users.users.${config.etu.user.username}.home = "/Users/${config.etu.user.username}";
  };
}
