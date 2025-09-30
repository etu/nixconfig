{
  description = "etu/nixpkgs";

  # Add all your dependencies here
  inputs = {
    # Packages
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-22-11.url = "github:NixOS/nixpkgs/nixos-22.11";

    # Hardware
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # Secrets
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.home-manager.follows = "home-manager";

    # Flake layout
    blueprint.url = "github:numtide/blueprint";
    blueprint.inputs.nixpkgs.follows = "nixpkgs";

    # Themes
    catppuccin.url = "github:catppuccin/nix";

    # Deployments
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";

    # Disk layouts
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    # Emacs packages
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    # Home-manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Impermanence
    impermanence.url = "github:nix-community/impermanence";

    # ip-failar-nu
    ip-failar-nu.url = "github:etu/ip.failar.nu";
    ip-failar-nu.inputs.nixpkgs.follows = "nixpkgs";

    # NUR
    nur.url = "nur";
  };

  outputs =
    inputs:

    # Call a library function to do a recursive merge to blueprint
    # with my own attributes.
    inputs.nixpkgs.lib.attrsets.recursiveUpdate

      # Load blueprint.
      (inputs.blueprint { inherit inputs; })

      # And depclare my own things that gets recursivly merged with
      # blueprint so they don't overwrite each other.
      (
        let
          # Functions to create deplayable nodes with deploy-rs
          mkDeploy =
            {
              name,
              hostname ? name,
              sshUser ? "root",
              system ? "x86_64-linux",
            }:
            {
              inherit sshUser hostname;
              profiles.system.path =
                inputs.deploy-rs.lib.${system}.activate.nixos
                  inputs.self.nixosConfigurations.${name};
            };

          # Declare deployable nodes
          deploy.nodes = {
            server-main-elis = mkDeploy { name = "server-main-elis"; };
            server-sparv = mkDeploy { name = "server-sparv"; };
            vps06 = mkDeploy { name = "vps06"; };
          };

          # This is highly advised, and will prevent many possible mistakes
          checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks deploy) inputs.deploy-rs.lib;
        in
        {
          inherit deploy checks;

          # Alias the live iso to a package.
          packages.x86_64-linux.iso = inputs.self.nixosConfigurations.live-iso.config.system.build.isoImage;
        }
      );
}
