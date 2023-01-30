{
  pkgs ? (import <nixpkgs> {}),
  inputs,
  system,
}:
pkgs.mkShell {
  buildInputs = [
    pkgs.cacert # Install certs for curl to work in pure shells
    pkgs.curl
    pkgs.jq # For parsing json downloaded with curl

    inputs.agenix.packages.${system}.agenix
    inputs.deploy-rs.packages.${system}.deploy-rs

    # Used for package updates of chalet
    pkgs.nodejs
    pkgs.nodePackages.node2nix

    # Used for firefox packages updates
    (pkgs.python3.withPackages (ps: [
      ps.requests
    ]))
  ];
}
