let
  # Load niv sources
  sources = import ../nix/sources.nix;
  nixus = import "${sources.nixus}/default.nix" { };
in
nixus ({ config, ... }: {
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

  nodes.fenchurch = { lib, config, ... }: {
    # How to reach this node
    host = "deploy@home.elis.nu";

    # What configuration it should have
    configuration = ../hosts/fenchurch/configuration.nix;
  };

  nodes.vps04 = { lib, config, ... }: {
    # How to reach this node
    host = "deploy@vps04.elis.nu";

    # What configuration it should have
    configuration = ../hosts/vps04/configuration.nix;
  };

  nodes.vps05 = { lib, config, ... }: {
    # How to reach this node
    host = "deploy@vps05.elis.nu";

    # What configuration it should have
    configuration = ../hosts/vps05/configuration.nix;
  };
})
