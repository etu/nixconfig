{
  config,
  flake,
  lib,
  pkgs,
  ...
}:
{
  options.etu.development.git.enable = lib.mkEnableOption "Enable development git settings";

  config = lib.mkIf config.etu.development.git.enable {
    # Install git system wide.
    environment.systemPackages = [
      pkgs.git
    ];

    # Configure git for my users home-manager.
    home-manager.users.${config.etu.user.username} = {
      imports = [
        flake.homeModules.git
      ];

      # Sign commits by default, with the SSH key through the ssh-agent.
      programs.git.settings.commit.gpgSign = true;
    };
  };
}
