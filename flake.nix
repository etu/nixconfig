{
  description = "etu/nixpkgs";

  inputs = {
    # Main nixpkgs channel
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Older nixpkgs for certain things.
    nixpkgs-22-11.url = "github:NixOS/nixpkgs/nixos-22.11";

    # Main flake-utils
    flake-utils.url = "github:numtide/flake-utils";

    # Catppuccin themes
    catppuccin.url = "github:catppuccin/nix";

    # Import deploy-rs for deployments
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.flake-compat.follows = "flake-compat";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.inputs.utils.follows = "flake-utils";

    # Import disko for disk partitioning
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    # Import flake-compat for follow of others that use it
    flake-compat.url = "github:edolstra/flake-compat";

    # Import nixos hardware quirks settings
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # Import impermanence modules for peristence
    impermanence.url = "github:nix-community/impermanence";

    # Import home-manager modules
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Import nixos-cosmic modules
    nixos-cosmic.url = "github:lilyinstarlight/nixos-cosmic";
    nixos-cosmic.inputs.flake-compat.follows = "flake-compat";
    nixos-cosmic.inputs.nixpkgs.follows = "nixpkgs";

    # Import nixos-needtoreboot
    nixos-needtoreboot.url = "github:thefossguy/nixos-needsreboot";
    nixos-needtoreboot.inputs.nixpkgs.follows = "nixpkgs";

    # Import agenix modules
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.home-manager.follows = "home-manager";

    # Import emacs-overlay
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";

    # Import ip-failar-nu
    ip-failar-nu.url = "github:etu/ip.failar.nu";
    ip-failar-nu.inputs.nixpkgs.follows = "nixpkgs";
    ip-failar-nu.inputs.flake-utils.follows = "flake-utils";

    # Import NUR
    nur.url = "nur";
  };

  outputs = {
    flake-utils,
    self,
    nixpkgs,
    ...
  } @ inputs: let
    myData = import ./data.nix;

    mkArmSystem = {
      name,
      extraArgs ? {},
      extraModules ? [],
    }:
      mkSystem {
        inherit name extraArgs extraModules;
        system = "aarch64-linux";
      };

    mkSystem = {
      name,
      system ? "x86_64-linux",
      extraArgs ? {},
      extraModules ? [],
    }: let
      pkgs-22-11 = import inputs.nixpkgs-22-11 {
        inherit system;
        config.allowUnfree = true;
      };
    in
      nixpkgs.lib.nixosSystem {
        inherit system;

        modules =
          [
            ./hosts/${name}/configuration.nix
            self.nixosModules.default
            inputs.agenix.nixosModules.age
            inputs.catppuccin.nixosModules.catppuccin
            inputs.disko.nixosModules.disko
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
            inputs.ip-failar-nu.nixosModules.${system}.default
            inputs.nixos-cosmic.nixosModules.default
            inputs.nur.nixosModules.nur
          ]
          ++ extraModules;

        specialArgs =
          {
            inherit (inputs) catppuccin;
            inherit (pkgs-22-11.nodejs-14_x.pkgs) intelephense;
            inherit (pkgs-22-11) chefdk;
            inherit (self.packages.${system}) swayWallpaper;
            inherit myData;

            emacs-overlay = inputs.emacs-overlay.overlay;
            emacsWayland = nixpkgs.legacyPackages.${system}.emacs29-pgtk;
            nixos-needsreboot = inputs.nixos-needtoreboot.packages.${system}.default;
          }
          // extraArgs;
      };

    mkArmDeploy = {
      name,
      hostname,
      sshUser ? "root",
    }:
      mkDeploy {
        inherit name hostname sshUser;
        system = "aarch64-linux";
      };

    mkDeploy = {
      name,
      hostname,
      sshUser ? "root",
      system ? "x86_64-linux",
    }: {
      inherit sshUser hostname;
      profiles.system.path = inputs.deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.${name};
    };
  in
    {
      # Declare systems
      nixosConfigurations = {
        laptop-private-caroline = mkSystem {name = "laptop-private-caroline";};
        laptop-private-elis = mkSystem {name = "laptop-private-elis";};
        laptop-work-elis = mkSystem {
          name = "laptop-work-elis";
          extraModules = [
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t14s-amd-gen1
          ];
        };
        server-main-elis = mkSystem {name = "server-main-elis";};
        vps06 = mkSystem {name = "vps06";};
        server-sparv = mkSystem {name = "server-sparv";};
        live-iso = mkSystem {name = "live-iso";};
      };

      # Specify deploy-rs deployments
      deploy.nodes = {
        server-main-elis = mkDeploy {
          name = "server-main-elis";
          hostname = "home.elis.nu";
        };
        server-sparv = mkDeploy {
          name = "server-sparv";
          hostname = "sparv.failar.nu";
        };
        vps06 = mkDeploy {
          name = "vps06";
          hostname = "vps06.elis.nu";
        };
      };

      # Expose my modules as a nixos module
      nixosModules.default = ./modules;

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;
    }
    // flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # Specify formatter package for "nix fmt ." and "nix fmt . -- --check"
      formatter = pkgs.alejandra;

      # Set up nix develop shell environment
      devShells.default = pkgs.mkShell {
        buildInputs = [
          pkgs.cacert # Install certs for curl to work in pure shells
          pkgs.curl
          pkgs.jq # For parsing json downloaded with curl

          # Linters
          pkgs.deadnix
          pkgs.statix
          pkgs.yamllint

          # Secrets managing
          inputs.agenix.packages.${system}.agenix

          # Deploy util
          inputs.deploy-rs.packages.${system}.deploy-rs
        ];
      };

      # Build packages
      packages.swayWallpaper = pkgs.callPackage ./packages/wallpaper {};
      packages.iso = self.nixosConfigurations.live-iso.config.system.build.isoImage;
    });
}
