{
  config,
  lib,
  ...
}:
{
  options.etu.user.username = lib.mkOption {
    type = lib.types.str;
    default = "etu";
    description = "My username for this system.";
  };

  config = {
    # User that user-specific options (defaults, homebrew, etc) apply to.
    system.primaryUser = config.etu.user.username;
    users.users.${config.etu.user.username}.home = "/Users/${config.etu.user.username}";
  };
}
