let
  # Load niv sources
  sources = import ../nix/sources.nix;
  nixusPath = "${sources.nixus}/default.nix";
in
import nixusPath { } ({ config, ... }: {
  # Set a nixpkgs version for all nodes
  defaults = { ... }: {
    nixpkgs = ../nix/nixos-unstable;
  };

  nodes.fenchurch = { lib, config, ... }: {
    # How to reach this node
    host = "deploy@home.elis.nu";

    # What configuration it should have
    configuration = ../hosts/fenchurch/configuration.nix;
  };
})
