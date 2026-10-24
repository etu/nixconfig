{ inputs, ... }:
{
  imports = [
    # New module organization
    ./base
    ./data
    ./development
    ./graphical
    ./hacks
    ./services
    ./user

    # Include modules imported from flakes
    inputs.agenix.nixosModules.age
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    inputs.impermanence.nixosModules.impermanence
    inputs.nur.modules.nixos.default
  ];
}
