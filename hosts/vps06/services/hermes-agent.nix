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
    #
    # Points at ollama on server-main-elis's Arc B580 (reachable over
    # tailscale) instead of the Anthropic API, to avoid further API spend.
    settings.model = {
      default = "llama3.1:8b";
      provider = "custom";
      base_url = "http://server-main-elis:11434/v1";
      # Matches OLLAMA_CONTEXT_LENGTH on server-main-elis -- Hermes requires
      # a served window of at least 64K.
      context_length = 65536;
    };
    settings.platforms.telegram.enabled = true;

    # Not secret, just gates who the bot will talk to -- TELEGRAM_BOT_TOKEN
    # lives in the agenix-managed environmentFiles secret instead.
    environment.TELEGRAM_ALLOWED_USERS = "574691347";
    environment.TELEGRAM_HOME_CHANNEL_THREAD_ID = "574691347";

    # Lets the `hermes` CLI be used interactively over SSH (chat/status/etc).
    addToSystemPackages = true;
  };
}
