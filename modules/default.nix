{ pkgs, ... }:

let
  # Load sources
  sources = import ../nix/sources.nix;
in {
  imports = [
    "${sources.agenix}/modules/age.nix"
    "${sources.flummbot}/nixos.nix"
    "${sources.home-manager}/nixos"
    "${sources.impermanence}/nixos.nix"
    "${sources.ip-failar-nu}/nixos.nix"
    ./my-allow-unfree.nix
    ./my-backup.nix
    ./my-common-cli.nix
    ./my-common-graphical.nix
    ./my-deploy-user.nix
    ./my-gaming.nix
    ./my-gpg-utils.nix
    ./my-nfsd.nix
    ./my-options.nix
    ./my-spell.nix
    ./my-sway.nix
    ./my-user.nix
    ./my-vbox.nix

    # New module organization
    ./base
    ./development
    ./graphical
    ./services
    ./user
  ];
}
