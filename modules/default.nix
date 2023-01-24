{ pkgs, inputs, system, ... }:

let
  # Load sources
  sources = import ../nix/sources.nix;

in
{
  imports = [
    inputs.agenix.nixosModules.age
    inputs.home-manager.nixosModules.home-manager
    inputs.impermanence.nixosModules.impermanence
    inputs.flummbot.nixosModules.${system}.default
    inputs.ip-failar-nu.nixosModules.${system}.default

    # New module organization
    ./base
    ./development
    ./games
    ./graphical
    ./services
    ./user
    ./work
  ];
}
