{ pkgs, inputs, ... }:

{
  imports = [
    "${inputs.impermanence}/nixos.nix"
    ./flummbot.nix
    ./ip-failar-nu.nix
    ./my-common-cli.nix
    ./my-common-graphical.nix
    ./my-desktop-gnome.nix
    ./my-emacs.nix
    ./my-gaming.nix
    ./my-gpg-utils.nix
    ./my-home-manager.nix
    ./my-i3.nix
    ./my-nfsd.nix
    ./my-spell.nix
    ./my-sway.nix
    ./my-update-config.nix
    ./my-user.nix
    ./my-vbox.nix
  ];
}
