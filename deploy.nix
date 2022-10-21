let
  # Load niv sources
  sources = import ./nix/sources.nix;
  nixus = import "${sources.nixus}/default.nix" { };
in
nixus {
  # Set a nixpkgs version for all nodes
  defaults.nixpkgs = ./nix/nixos-unstable;

  nodes.home-server = {
    host = "root@home.elis.nu";
    configuration = ./hosts/home-server/configuration.nix;
  };

  nodes.vps04 = {
    host = "root@vps04.elis.nu";
    configuration = ./hosts/vps04/configuration.nix;
  };

  nodes.vps06 = {
    host = "root@vps06.elis.nu";
    configuration = ./hosts/vps06/configuration.nix;
  };
}
