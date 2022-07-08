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
    ./my-backup.nix
    ./my-common-cli.nix
    ./my-deploy-user.nix
    ./my-options.nix
    ./my-spell.nix

    # New module organization
    ./base
    ./development
    ./games
    ./graphical
    ./services
    ./user
  ];
}
