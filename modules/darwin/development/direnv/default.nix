{
  config,
  lib,
  ...
}:
{
  options.etu.development.direnv.enable = lib.mkEnableOption "Enable development direnv settings";

  config = lib.mkIf config.etu.development.direnv.enable {
    home-manager.users.${config.etu.user.username}.programs.direnv = {
      # direnv with nix-direnv for cached `use flake` environments (the NixOS
      # module uses lorri instead, which isn't available as a service here).
      enable = true;
      nix-direnv.enable = true;

      # The shared fish module already sets up the direnv hook itself.
      enableFishIntegration = false;
    };
  };
}
