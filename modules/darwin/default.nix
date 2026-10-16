{ inputs, ... }:
{
  imports = [
    ./base
    ./development
    ./graphical
    ./user

    # Include modules imported from flakes
    inputs.home-manager.darwinModules.home-manager
    inputs.nur.modules.darwin.default
  ];
}
