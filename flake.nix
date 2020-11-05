{
  description = "etu/nixconfig";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    wayland.url = "github:colemickens/nixpkgs-wayland";
    impermanence = {
      flake = false;
      url = "https://github.com/nix-community/impermanence/archive/8fc761e8c34.tar.gz";
     };
  };

  outputs = inputs:
    let
      mkSystem = system: pkgs': hostname:
        pkgs'.lib.nixosSystem {
          inherit system;
          modules = [ (./. + "/hosts/${hostname}/configuration.nix") ];
          specialArgs = { inherit inputs; };
        };
    in
    {
      nixosConfigurations.agrajag = mkSystem "x86_64-linux" inputs.nixpkgs "agrajag";
      nixosConfigurations.fenchurch = mkSystem "x86_64-linux" inputs.nixpkgs "fenchurch";

      devShell.x86_64-linux = import ./shell.nix { pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux; };
      devShell.aarch64-linux = import ./shell.nix { pkgs = inputs.nixpkgs.legacyPackages.aarch64-linux; };
    };
}
