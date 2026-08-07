{
  config,
  flake,
  ...
}:
{
  imports = [
    flake.inputs.hermes-agent.nixosModules.default
  ];

  age.secrets = {
    inherit (config.etu.data.ageModules) hermes-agent-env;
  };

  etu.base.zfs.system.directories = [
    # Persist agent state (memory, sessions, skills, config) across reboots
    "/var/lib/hermes"
  ];

  services.hermes-agent = {
    enable = true;
    environmentFiles = [ config.age.secrets.hermes-agent-env.path ];

    # Config is declarative in managed mode (HERMES_MANAGED=true) -- the
    # `hermes setup`/`hermes config set` wizards refuse to run and point
    # back here instead.
    settings.model.default = "anthropic/claude-sonnet-5";
    settings.platforms.telegram.enabled = true;

    # Not secret, just gates who the bot will talk to -- TELEGRAM_BOT_TOKEN
    # lives in the agenix-managed environmentFiles secret instead.
    environment.TELEGRAM_ALLOWED_USERS = "574691347";
    environment.TELEGRAM_HOME_CHANNEL_THREAD_ID = "574691347";

    # Lets the `hermes` CLI be used interactively over SSH (chat/status/etc).
    addToSystemPackages = true;
  };
}
