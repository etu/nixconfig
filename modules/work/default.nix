{
  config,
  chefdk,
  lib,
  pkgs,
  vagrant,
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

    # Persist directories and files
    etu.base.zfs.user.directories = [
      ".chalet"
      ".chef"
      ".config/Slack"
      ".config/google-chrome"
      ".config/helm"
      ".config/tvnu"
      ".vagrant.d"
    ];

    etu.base.zfs.user.files = [
      ".docker/config.json"
      ".kube/config"
    ];

    # Allow certain unfree packages.
    etu.base.nix.allowUnfree = [
      "appgate-sdp"
      "google-chrome"
      "slack"
    ];

    etu.user.extraUserPackages = [
      # Install chalet to manage running of containers
      config.nur.repos.etu.chalet

      # Install github-markdown-toc to format README TOC's
      config.nur.repos.etu.github-markdown-toc

      # Install make
      pkgs.gnumake

      # Install git crypt
      pkgs.git-crypt

      # Install chef and vagrant for some legacy systems reasons
      chefdk
      vagrant

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
