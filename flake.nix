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
  in
    {
      # Declare systems
      nixosConfigurations = {
        laptop-private-elis = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/laptop-private-elis/configuration.nix
            ./modules
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t495
            inputs.agenix.nixosModules.age
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
          ];
          specialArgs = {
            inherit myData;
            inherit (intelephense.nodejs-14_x.pkgs) intelephense;
            inherit (self.packages.${system}) swayWallpaper g90updatefw etuvetica talyznewroman;
            emacsWayland = inputs.emacs-overlay.packages.${system}.emacsPgtk;
            llr = inputs.llr.packages.${system}.default;
            mkvcleaner = inputs.mkvcleaner.packages.${system}.default;
            emacs-overlay = inputs.emacs-overlay.overlay;
          };
        };

        laptop-work-elis = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/laptop-work-elis/configuration.nix
            ./modules
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t14s-amd-gen1
            inputs.agenix.nixosModules.age
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
          ];
          specialArgs = {
            inherit myData;
            inherit (intelephense.nodejs-14_x.pkgs) intelephense;
            inherit (self.packages.${system}) swayWallpaper etuvetica talyznewroman;
            emacsWayland = inputs.emacs-overlay.packages.${system}.emacsPgtk;
            llr = inputs.llr.packages.${system}.default;
            mkvcleaner = inputs.mkvcleaner.packages.${system}.default;
            emacs-overlay = inputs.emacs-overlay.overlay;
          };
        };

        server-main-elis = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/server-main-elis/configuration.nix
            ./modules
            inputs.agenix.nixosModules.age
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
          ];
          specialArgs = {
            inherit myData;
            inherit (intelephense.nodejs-14_x.pkgs) intelephense;
            llr = inputs.llr.packages.${system}.default;
            mkvcleaner = inputs.mkvcleaner.packages.${system}.default;
            emacs-overlay = inputs.emacs-overlay.overlay;
          };
        };

        vps04 = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/vps04/configuration.nix
            ./modules
            inputs.agenix.nixosModules.age
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
            inputs.flummbot.nixosModules.${system}.default
          ];
          specialArgs = {
            inherit myData;
            llr = inputs.llr.packages.${system}.default;
            mkvcleaner = inputs.mkvcleaner.packages.${system}.default;
          };
        };

        vps06 = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/vps06/configuration.nix
            ./modules
            inputs.agenix.nixosModules.age
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
            inputs.ip-failar-nu.nixosModules.${system}.default
          ];
          specialArgs = {
            inherit myData;
            llr = inputs.llr.packages.${system}.default;
            mkvcleaner = inputs.mkvcleaner.packages.${system}.default;
            via-elis-nu = inputs.via-elis-nu.packages.${system}.default;
          };
        };

        live-iso = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/live-iso/configuration.nix
            ./modules
            inputs.agenix.nixosModules.age
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
          ];
          specialArgs = {
            inherit myData;
            inherit (intelephense.nodejs-14_x.pkgs) intelephense;
            inherit (self.packages.${system}) swayWallpaper etuvetica talyznewroman;
            emacsWayland = inputs.emacs-overlay.packages.${system}.emacsPgtk;
            llr = inputs.llr.packages.${system}.default;
            mkvcleaner = inputs.mkvcleaner.packages.${system}.default;
            emacs-overlay = inputs.emacs-overlay.overlay;
          };
        };
      };

      # Specify deploy-rs deployments
      deploy.nodes = {
        server-main-elis = {
          hostname = "home.elis.nu";
          sshUser = "root";
          profiles.system.path = inputs.deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.server-main-elis;
        };

        vps04 = {
          hostname = "vps04.elis.nu";
          sshUser = "root";
          profiles.system.path = inputs.deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.vps04;
        };

        vps06 = {
          hostname = "vps06.elis.nu";
          sshUser = "root";
          profiles.system.path = inputs.deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.vps06;
        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;
    }
    // flake-utils.lib.eachDefaultSystem (system: let
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

          inputs.agenix.packages.${system}.agenix
          inputs.deploy-rs.packages.${system}.deploy-rs

          # Used for package updates of chalet
          pkgs.nodejs
          pkgs.nodePackages.node2nix

          # Used for firefox packages updates
          (pkgs.python3.withPackages (ps: [
            ps.requests
          ]))
        ];
      };

      # Build packages
      packages = {
        g90updatefw = pkgs.callPackage ./packages/g90updatefw {};
        swayWallpaper = pkgs.callPackage ./packages/wallpaper {};
        etuvetica = pkgs.callPackage ./packages/etuvetica {};
        talyznewroman = pkgs.callPackage ./packages/talyznewroman {};
      };
    });
}
