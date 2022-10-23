{ pkgs ? (import <nixpkgs> { }) }:

let
  # Load sources
  agenix = pkgs.callPackage "${(import ./nix/sources.nix).agenix}/pkgs/agenix.nix" { };
in
pkgs.mkShell {
  buildInputs = [
    agenix

    pkgs.cacert # Install certs for curl to work in pure shells
    pkgs.curl
    pkgs.jq     # For parsing json downloaded with curl

    pkgs.niv
    pkgs.nixpkgs-fmt
  ];
}
