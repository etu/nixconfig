{
  config,
  lib,
  ...
}:
{
  options.etu.base.ssh.enable = lib.mkEnableOption "Enable base ssh client settings";

  config = lib.mkIf config.etu.base.ssh.enable {
    home-manager.users.${config.etu.user.username} = {
      # Use the ssh-agent macOS already runs per login session (SSH_AUTH_SOCK
      # is set by launchd), with key passphrases stored in the Keychain. Store
      # the passphrase once with:
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

      # The agent starts empty at login and only ssh itself adds keys on first
      # use, so git commit signing would prompt for the passphrase after a
      # reboot. Load the keys from the Keychain into the agent at login instead.
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
    };
  };
}
