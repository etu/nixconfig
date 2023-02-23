{
  description = "etu/nixpkgs";

  inputs = {
    # Main nixpkgs channel
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Main flake-utils
    flake-utils.url = "flake-utils";

    # A nixpkgs for intelephense.
    nixpkgs-intelephense.url = "github:NixOS/nixpkgs/befc83905c965adfd33e5cae49acb0351f6e0404";

    # Import deploy-rs for deployments
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.inputs.utils.follows = "flake-utils";

    # Import nixos hardware quirks settings
    nixos-hardware.url = "nixos-hardware";

    # Import impermanence modules for peristence
    impermanence.url = "github:nix-community/impermanence";

    # Import home-manager modules
    home-manager.url = "home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.utils.follows = "flake-utils";

    # Import agenix modules
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    # Import emacs-overlay
    emacs-overlay.url = "emacs-overlay";
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

    # Import via-elis-nu, don't follow upstream nixpkgs to avoid
    # unwanted changes and breakage.
    via-elis-nu.url = "github:etu/via.elis.nu";
    via-elis-nu.inputs.flake-utils.follows = "flake-utils";

    # Import my NUR packages that aren't part of NUR yet
    etu-nur.url = "github:etu/nur-packages";
    etu-nur.inputs.nixpkgs.follows = "nixpkgs";

    # Import NUR
    nur.url = "nur";
  };

  outputs = {
    flake-utils,
    self,
    nixpkgs,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    myData = import ./data.nix;
    intelephense = import inputs.nixpkgs-intelephense {
      inherit system;
      config.allowUnfree = true;
    };

    mkSystem = {
      name,
      system ? "x86_64-linux",
      extraArgs ? {},
      extraModules ? [],
    }:
      nixpkgs.lib.nixosSystem {
        inherit system;

        modules =
          [
            ./hosts/${name}/configuration.nix
            ./modules
            inputs.agenix.nixosModules.age
            inputs.flummbot.nixosModules.${system}.default
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
            inputs.ip-failar-nu.nixosModules.${system}.default
            inputs.nur.nixosModules.nur
          ]
          ++ extraModules;

        specialArgs =
          {
            inherit (inputs.etu-nur.packages.${system}) chalet firefox-extension-elasticvue firefox-extension-streetpass-for-mastodon font-etuvetica font-talyznewroman g90updatefw llr matrix-hookshot mkvcleaner;
            inherit (intelephense.nodejs-14_x.pkgs) intelephense;
            inherit (self.packages.${system}) swayWallpaper;
            inherit myData;
            emacs-overlay = inputs.emacs-overlay.overlay;
            emacsWayland = inputs.emacs-overlay.packages.${system}.emacsPgtk;
            via-elis-nu = inputs.via-elis-nu.packages.${system}.default;
          }
          // extraArgs;
      };

    mkDeploy = {
      name,
      hostname,
      sshUser ? "root",
    }: {
      inherit sshUser hostname;
      profiles.system.path = inputs.deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.${name};
    };
  in
    {
      # Declare systems
      nixosConfigurations = {
        laptop-private-elis = mkSystem {
          name = "laptop-private-elis";
          extraModules = [
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t495
          ];
        };
        laptop-work-elis = mkSystem {
          name = "laptop-work-elis";
          extraModules = [
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t14s-amd-gen1
          ];
        };
        server-main-elis = mkSystem {name = "server-main-elis";};
        vps04 = mkSystem {name = "vps04";};
        vps06 = mkSystem {name = "vps06";};
        live-iso = mkSystem {name = "live-iso";};
      };

      # Specify deploy-rs deployments
      deploy.nodes = {
        server-main-elis = mkDeploy {
          name = "server-main-elis";
          hostname = "home.elis.nu";
        };
        vps04 = mkDeploy {
          name = "vps04";
          hostname = "vps04.elis.nu";
        };
        vps06 = mkDeploy {
          name = "vps06";
          hostname = "vps06.elis.nu";
        };
      };

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

          # Install nix-output-monitor for nice build commands
          pkgs.nix-output-monitor

          inputs.agenix.packages.${system}.agenix
          inputs.deploy-rs.packages.${system}.deploy-rs
        ];
      };

      # Build packages
      packages.swayWallpaper = pkgs.callPackage ./packages/wallpaper {};
    });
}
