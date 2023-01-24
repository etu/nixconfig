{ pkgs ? (import <nixpkgs> { }), inputs, system }:

pkgs.mkShell {
  buildInputs = [
    pkgs.cacert # Install certs for curl to work in pure shells
    pkgs.curl
    pkgs.jq     # For parsing json downloaded with curl

    pkgs.nixpkgs-fmt

    inputs.agenix.packages.${system}.agenix
    inputs.deploy-rs.packages.${system}.deploy-rs
  ];
}
