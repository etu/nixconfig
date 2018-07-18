{ config, pkgs, ... }:

{
  imports = [
    ./ip-failar-nu.nix
    ./my-common-cli.nix
    ./my-common-graphical.nix
    ./my-gpg-utils.nix
    ./my-user.nix
  ];
}
