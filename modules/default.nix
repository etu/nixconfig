{inputs, ...}: {
  imports = [
    # New module organization
    ./base
    ./data
    ./development
    ./games
    ./graphical
    ./services
    ./theme
    ./user
    ./work

    # Include modules imported from flakes
    inputs.agenix.nixosModules.age
    inputs.catppuccin.nixosModules.catppuccin
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    inputs.impermanence.nixosModules.impermanence
    inputs.nur.modules.nixos.default
  ];
}
