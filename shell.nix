{ pkgs ? (import <nixpkgs> { }) }:

let
  # Load sources
  agenix = pkgs.callPackage "${(import ./nix/sources.nix).agenix}/pkgs/agenix.nix" { };
in
pkgs.mkShell {
  buildInputs = [
    agenix

    pkgs.niv
    pkgs.nixpkgs-fmt
  ];
}
