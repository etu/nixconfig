{ pkgs, ... }:

let
  # Load sources
  sources = import ../nix/sources.nix;
in {
  imports = [
    "${sources.agenix}/modules/age.nix"
    "${sources.impermanence}/nixos.nix"
    "${sources.ip-failar-nu}/nixos.nix"
    "${sources.flummbot}/nixos.nix"
    ./my-backup.nix
    ./my-common-cli.nix
    ./my-common-graphical.nix
    ./my-deploy-user.nix
    ./my-emacs.nix
    ./my-fonts.nix
    ./my-gaming.nix
    ./my-gpg-utils.nix
    ./my-home-manager.nix
    ./my-nfsd.nix
    ./my-options.nix
    ./my-spell.nix
    ./my-sway.nix
    ./my-user.nix
    ./my-vbox.nix
  ];

  # Pull in the 5.14 kernel from an older nixpkgs to get specifically
  # the 5.14.14 kernel since 5.14.15 seems to have some odd hardware
  # issues on my laptop.
  nixpkgs.overlays = let
    kpkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/34ad3ff.tar.gz";
      sha256 = "02li241rz5668nfyp88zfjilxf0mr9yansa93fbl38hjwkhf3ix6";
    }) { };
  in [
    (self: super: {
      linuxPackages_5_14 = kpkgs.linuxPackages_5_14;
    })
  ];
}
