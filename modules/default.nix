{ config, pkgs, ... }:

{
  imports = [
    ./flummbot.nix
    ./impermanence.nix
    ./ip-failar-nu.nix
    ./my-common-cli.nix
    ./my-common-graphical.nix
    ./my-desktop-gnome.nix
    ./my-emacs.nix
    ./my-gaming.nix
    ./my-gpg-utils.nix
    ./my-home-manager.nix
    ./my-nfsd.nix
    ./my-spell.nix
    ./my-update-config.nix
    ./my-user.nix
    ./my-vbox.nix
  ];
}
