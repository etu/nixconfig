{ config, pkgs, ... }:

{
  imports = [
    ./modules/ip-failar-nu.nix
    ./modules/my-common-cli.nix
    ./modules/my-common-graphical.nix
    ./modules/my-desktop-gnome.nix
    ./modules/my-dmrconfig.nix
    ./modules/my-emacs.nix
    ./modules/my-gaming.nix
    ./modules/my-gpg-utils.nix
    ./modules/my-nfsd.nix
    ./modules/my-update-config.nix
    ./modules/my-user.nix
    ./modules/my-vbox.nix
  ];

  # Local overlays
  nixpkgs.overlays = [
    (import ./pkgs/default.nix)
  ];
}
