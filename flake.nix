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

    # Import agenix modules
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.home-manager.follows = "home-manager";

    # Import emacs-overlay
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

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
            inputs.ip-failar-nu.nixosModules.${system}.default
          ]
          ++ extraModules;

        specialArgs =
          {
            inherit inputs;
            inherit (pkgs-22-11.nodejs-14_x.pkgs) intelephense;
            inherit (pkgs-22-11) chefdk vagrant;
            inherit (self.packages.${system}) spaceWallpapers nixosSystemdKexec;
            inherit myData;

            emacs-overlay = inputs.emacs-overlay.overlay;
            emacsWayland = nixpkgs.legacyPackages.${system}.emacs30-pgtk;
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
        desktop-caroline = mkSystem {name = "desktop-caroline";};
        desktop-elis = mkSystem {name = "desktop-elis";};
        laptop-private-caroline = mkSystem {name = "laptop-private-caroline";};
        laptop-private-elis = mkSystem {name = "laptop-private-elis";};
        laptop-work-elis = mkSystem {name = "laptop-work-elis";};
        server-main-elis = mkSystem {name = "server-main-elis";};
        vps06 = mkSystem {name = "vps06";};
        server-sparv = mkSystem {name = "server-sparv";};
        live-iso = mkSystem {name = "live-iso";};
      };

      # Specify deploy-rs deployments
      deploy.nodes = {
        server-main-elis = mkDeploy {
          name = "server-main-elis";
          hostname = "server-main-elis";
        };
        server-sparv = mkDeploy {
          name = "server-sparv";
          hostname = "server-sparv";
        };
        vps06 = mkDeploy {
          name = "vps06";
          hostname = "vps06";
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
      devShells.default = pkgs.callPackage ./devshell.nix {flake = self;};

      # Build packages
      packages.spaceWallpapers = pkgs.callPackage ./packages/spaceWallpapers {};
      packages.nixosSystemdKexec = pkgs.callPackage ./packages/nixosSystemdKexec {};
      packages.iso = self.nixosConfigurations.live-iso.config.system.build.isoImage;

      # Expose commands/programs under nix run .#foo
      apps.vcodeGetLatestExtensions = {
        type = "app";
        program = "${(pkgs.callPackage ./packages/vscodeGetLatestExtensions {})}/bin/vscode-get-latest-extensions";
        meta.description = "Get latest compatible VSCode extension for specified version of VSCode";
      };
    });
}
