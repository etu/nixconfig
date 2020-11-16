{
  description = "etu/nixconfig";

  inputs = {
    # Emacs Overlay
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # Import my irc-bot as a flake
    flummbot.url = "github:etu/flummbot";
    flummbot.inputs.nixpkgs.follows = "nixpkgs";

    # Import my tiny web service to respond with the client IP
    ip-failar-nu.url = "github:etu/ip.failar.nu";
    ip-failar-nu.inputs.nixpkgs.follows = "nixpkgs";

    # Home Manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # NixOS hardware quirks
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    # Main nixpkgs channel
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Wayland overlay
    wayland.url = "github:colemickens/nixpkgs-wayland";
    wayland.inputs.nixpkgs.follows = "nixpkgs";

    # Persistance things
    impermanence.url = "https://github.com/nix-community/impermanence/archive/8fc761e8c34.tar.gz";
    impermanence.flake = false;
  };

  outputs = inputs: let
    mkSystem = system: pkgs': hostname:
      pkgs'.lib.nixosSystem {
        inherit system;
        modules = [ (./. + "/hosts/${hostname}/configuration.nix") ];
        specialArgs = { inherit inputs; };
      };
  in {
    nixosConfigurations.agrajag = mkSystem "x86_64-linux" inputs.nixpkgs "agrajag";
    nixosConfigurations.eliaxe-59087-t480s = mkSystem "x86_64-linux" inputs.nixpkgs "eliaxe-59087-t480s";
    nixosConfigurations.fenchurch = mkSystem "x86_64-linux" inputs.nixpkgs "fenchurch";
    nixosConfigurations.kodi = mkSystem "x86_64-linux" inputs.nixpkgs "kodi";
    nixosConfigurations.vps04 = mkSystem "x86_64-linux" inputs.nixpkgs "vps04";
    nixosConfigurations.vps05 = mkSystem "x86_64-linux" inputs.nixpkgs "vps05";

    devShell.x86_64-linux = import ./shell.nix { pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux; };
    devShell.aarch64-linux = import ./shell.nix { pkgs = inputs.nixpkgs.legacyPackages.aarch64-linux; };
  };
}
