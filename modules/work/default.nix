{
  config,
  chalet,
  lib,
  pkgs,
  ...
}: {
  options.etu.work.enable = lib.mkEnableOption "Enables work module";

  config = lib.mkIf config.etu.work.enable {
    # Configure PHP to be the correct version with the right extensions
    nixpkgs.overlays = [
      (self: super: {
        php = pkgs.php81.withExtensions (
          {
            all,
            enabled,
          }:
            enabled ++ (with all; [imagick memcached redis pcov])
        );
      })
    ];

    # Persist google chrome config directory
    etu.base.zfs.user.directories = [
      ".config/google-chrome"
    ];

    # Allow certain unfree packages.
    etu.base.nix.allowUnfree = [
      "appgate-sdp"
      "google-chrome"
      "slack"
    ];

    etu.user.extraUserPackages = [
      # Install chalet to manage running of containers
      chalet

      # Install make
      pkgs.gnumake

      # Install chef and vagrant for some legacy systems reasons
      pkgs.chefdk
      pkgs.vagrant

      # Kubernetes and Docker utils
      pkgs.docker-compose
      pkgs.kubectl
      pkgs.kubectx
      pkgs.kubetail
      pkgs.kubent
      pkgs.kubernetes-helm
      pkgs.minikube
      pkgs.octant

      # Misc
      pkgs.google-chrome
      pkgs.mariadb
      pkgs.slack
    ];

    # Enable appgate
    programs.appgate-sdp.enable = true;
  };
}
