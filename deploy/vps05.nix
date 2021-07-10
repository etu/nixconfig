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

  nodes.vps05 = { lib, config, ... }: {
    # How to reach this node
    host = "deploy@vps05.elis.nu";

    # What configuration it should have
    configuration = ../hosts/vps05/configuration.nix;
  };
})
