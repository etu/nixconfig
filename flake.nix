{
  description = "etu/nixpkgs";

  inputs = {
    # Main nixpkgs channel
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Main flake-utils
    flake-utils.url = "github:numtide/flake-utils";

    # A nixpkgs for intelephense.
    nixpkgs-intelephense.url = "github:NixOS/nixpkgs/befc83905c965adfd33e5cae49acb0351f6e0404";

    # Import deploy-rs for deployments
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.inputs.utils.follows = "flake-utils";

    # Import nixos hardware quirks settings
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    # Import impermanence modules for peristence
    impermanence.url = "github:nix-community/impermanence";

    # Import home-manager modules
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.utils.follows = "flake-utils";

    # Import agenix modules
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    # Import emacs-overlay
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";

    # Import flummbot
    flummbot.url = "github:etu/flummbot";
    flummbot.inputs.nixpkgs.follows = "nixpkgs";
    flummbot.inputs.flake-utils.follows = "flake-utils";

    # Import ip-failar-nu
    ip-failar-nu.url = "github:etu/ip.failar.nu";
    ip-failar-nu.inputs.nixpkgs.follows = "nixpkgs";
    ip-failar-nu.inputs.flake-utils.follows = "flake-utils";

    # Import mkvcleaner
    mkvcleaner.url = "github:etu/mkvcleaner";
    mkvcleaner.inputs.nixpkgs.follows = "nixpkgs";
    mkvcleaner.inputs.flake-utils.follows = "flake-utils";

    # Import llr
    llr.url = "github:etu/llr";
    llr.inputs.nixpkgs.follows = "nixpkgs";
    llr.inputs.flake-utils.follows = "flake-utils";

    # Import via-elis-nu, don't follow upstream nixpkgs to avoid
    # unwanted changes and breakage.
    via-elis-nu.url = "github:etu/via.elis.nu";
    via-elis-nu.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, deploy-rs, ... }@inputs: {
    # Declare systems
    nixosConfigurations = {
      laptop-private-elis = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/laptop-private-elis/configuration.nix ./modules ];
        specialArgs = { inherit inputs; system = "x86_64-linux"; };
      };

      laptop-work-elis = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/laptop-work-elis/configuration.nix ./modules ];
        specialArgs = { inherit inputs; system = "x86_64-linux"; };
      };

      server-main-elis = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/server-main-elis/configuration.nix ./modules ];
        specialArgs = { inherit inputs; system = "x86_64-linux"; };
      };

      vps04 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/vps04/configuration.nix ./modules ];
        specialArgs = { inherit inputs; system = "x86_64-linux"; };
      };

      vps06 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/vps06/configuration.nix ./modules ];
        specialArgs = { inherit inputs; system = "x86_64-linux"; };
      };

      live-iso = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./hosts/live-iso/configuration.nix ./modules ];
        specialArgs = { inherit inputs; system = "x86_64-linux"; };
      };
    };

    # Set up nix develop shell environment
    devShells.x86_64-linux.default = let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in import ./shell.nix { inherit pkgs; inherit (self) inputs; system = "x86_64-linux"; };

    # Specify deploy-rs deployments
    deploy.nodes = {
      server-main-elis = {
        hostname = "home.elis.nu";
        sshUser = "root";
        profiles.system.path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.server-main-elis;
      };

      vps04 = {
        hostname = "vps04.elis.nu";
        sshUser = "root";
        profiles.system.path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.vps04;
      };

      vps06 = {
        hostname = "vps06.elis.nu";
        sshUser = "root";
        profiles.system.path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.vps06;
      };
    };

    # This is highly advised, and will prevent many possible mistakes
    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
  };
}
