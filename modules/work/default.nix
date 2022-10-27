{ config, lib, pkgs, ... }:
let
  myPkgs = pkgs.callPackage ./packages { };

in {
  options.etu.work.enable = lib.mkEnableOption "Enables work module";

  config = lib.mkIf config.etu.work.enable {
    # Configure PHP to be the correct version with the right extensions
    nixpkgs.overlays = [
      (self: super: {
        php = pkgs.php81.withExtensions ({ all, enabled }:
          enabled ++ (with all; [ imagick memcached redis pcov ])
        );
      })
    ];

    # Allow certain unfree packages.
    etu.base.nix.allowUnfree = [
      "appgate-sdp"
    ];

    etu.user.extraUserPackages = [
      # Install chalet to manage running of containers
      myPkgs.chalet

      # Install make
      pkgs.gnumake
    ];

    # Enable appgate
    programs.appgate-sdp.enable = true;
  };
}
