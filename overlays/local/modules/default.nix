{ config, pkgs, ... }:

{
  imports = [
    ./ip-failar-nu.nix
    ./my-common-cli.nix
    ./my-common-graphical.nix
    ./my-desktop-gnome.nix
    ./my-gaming.nix
    ./my-gpg-utils.nix
    ./my-user.nix
    ./my-vbox.nix
  ];
}
