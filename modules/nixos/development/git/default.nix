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

    # Configure git for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.git
      ];

      # Don't sign commits by default, the NixOS hosts have no signing setup.
      programs.git.settings.commit.gpgSign = false;
    };
  };
}
