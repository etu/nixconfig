{ config, lib, ... }:

{
  imports = [
    ./fish
    ./htop
  ];

  options.etu.stateVersion = lib.mkOption {
    example = "22.05";
    description = "The NixOS state version to use for this system";
  };

  config = {
    # Enable base services.
    etu.base = {
      fish.enable = true;
      htop.enable = true;
    };

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
