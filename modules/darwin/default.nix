{ inputs, ... }:
{
  imports = [
    ./base
    ./development
    ./graphical
    ./user

    # Include modules imported from flakes
    inputs.nur.modules.darwin.default
  ];
}
