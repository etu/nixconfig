{
  pkgs,
  flake,
  system,
}:
pkgs.mkShell {
  # Add build dependencies
  packages = [
    pkgs.cacert # Install certs for curl to work in pure shells
    pkgs.curl
    pkgs.jq # For parsing json downloaded with curl

    # Linters
    pkgs.deadnix
    pkgs.statix
    pkgs.yamllint

    # Secrets managing
    flake.inputs.agenix.packages.${system}.agenix

    # Deploy util
    flake.inputs.deploy-rs.packages.${system}.deploy-rs
  ];
}
