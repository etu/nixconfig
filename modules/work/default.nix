{ config, lib, pkgs, ... }:
let
  myPkgs = pkgs.callPackage ./packages { };

in {
  options.etu.work.enable = lib.mkEnableOption "Enables work module";

  config = lib.mkIf config.etu.work.enable {
    etu.user.extraUserPackages = [
      # Install chalet to manage running of containers
      myPkgs.chalet

      # Install make
      pkgs.gnumake
    ];
  };
}
