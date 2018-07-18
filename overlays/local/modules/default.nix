{ config, pkgs, ... }:

{
  imports = [
    ./ip-failar-nu.nix
    ./my-common-cli.nix
    ./my-gpg-utils.nix
  ];
}
