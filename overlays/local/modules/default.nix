{ config, pkgs, ... }:

{
  imports = [
    ./ip-failar-nu.nix
    ./my-gpg-utils.nix
  ];
}
