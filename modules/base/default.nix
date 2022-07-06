{ config, lib, ... }:

{
  options.etu.stateVersion = lib.mkOption {
    example = "22.05";
    description = "The NixOS state version to use for this system";
  };

  config = {
    # Set system state version.
    system.stateVersion = config.etu.stateVersion;

    # Set state version for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.stateVersion = config.etu.stateVersion;
    };

    # Set state version for root users home-manager.
    home-manager.users.root.home.stateVersion = config.etu.stateVersion;
  };
}
