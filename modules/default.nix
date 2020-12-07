{ pkgs, ... }:

let
  # Load sources
  sources = import ../nix/sources.nix;
in {
  imports = [
    "${sources.impermanence}/nixos.nix"
    "${sources.ip-failar-nu}/nixos.nix"
    #inputs.flummbot.nixosModule
    ./my-auto-upgrade.nix
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
    ./my-user.nix
    ./my-vbox.nix
  ];
}
