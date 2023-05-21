{
  description = "etu/nixpkgs";

  inputs = {
    # Main nixpkgs channel
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Older nixpkgs for certain things.
    nixpkgs-22-11.url = "github:NixOS/nixpkgs/nixos-22.11";

    # Main flake-utils
    flake-utils.url = "flake-utils";

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

    # Import agenix modules
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.home-manager.follows = "home-manager";

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
            inherit (pkgs-22-11.nodejs-14_x.pkgs) intelephense;
            inherit (pkgs-22-11) chefdk ansible_2_12;
            inherit (self.packages.${system}) swayWallpaper;
            inherit myData;
            emacs-overlay = inputs.emacs-overlay.overlay;
            emacsWayland = inputs.emacs-overlay.packages.${system}.emacsPgtk;
            via-elis-nu = inputs.via-elis-nu.packages.${system}.default;
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
        server-sparv = mkSystem {name = "server-sparv";};
        live-iso = mkSystem {name = "live-iso";};
        pi-octonix = mkArmSystem {name = "pi-octonix";};
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
        #pi-octonix = mkArmDeploy {
        #  name = "pi-octonix";
        #  hostname = "octonix.lan";
        #};
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
