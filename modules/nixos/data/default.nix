{ lib, ... }:
{
  options.etu.data = lib.mkOption {
    default = import ../../../data.nix;
  };
}
