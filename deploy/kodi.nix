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

  nodes.kodi = { lib, config, ... }: {
    # How to reach this node
    host = "root@192.168.0.105";

    # What configuration it should have
    configuration = ../hosts/kodi/configuration.nix;
  };
})
