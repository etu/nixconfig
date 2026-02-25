{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:
{
  options.etu.work.enable = lib.mkEnableOption "Enables work module";

  config = lib.mkIf config.etu.work.enable {
    # Configure PHP to be the correct version with the right extensions
    nixpkgs.overlays = [
      (_self: _super: {
        php = pkgs.php84.withExtensions (
          {
            all,
            enabled,
          }:
          enabled
          ++ (with all; [
            imagick
            memcached
            redis
            pcov
            protobuf
          ])
        );
      })
    ];

    # Persist directories and files
    etu.base.zfs.user.directories = [
      ".aws"
      ".chalet"
      ".chef"
      ".config/helm"
      ".config/tvnu"
      ".config/gh"
      ".vagrant.d"
    ];

    etu.base.zfs.user.files = [
      ".config/goprocmgr.json"
      ".docker/config.json"
      ".kube/config"
    ];

    # Allow certain unfree packages.
    etu.base.nix.allowUnfree = [
      "appgate-sdp"
      "vagrant"
      "terraform"
    ];

    etu.user.extraUserPackages = [
      # Install chalet to manage running of containers
      pkgs.nur.repos.etu.goprocmgr
      pkgs.nur.repos.etu.chalet

      # Install github-markdown-toc to format README TOC's
      pkgs.nur.repos.etu.github-markdown-toc

      # Install make
      pkgs.gnumake

      # Install git crypt
      pkgs.git-crypt
      pkgs.git-lfs
      pkgs.github-cli

      # Install chef and vagrant for some legacy systems reasons
      perSystem.nixpkgs-22-11.chefdk
      perSystem.nixpkgs-22-11.vagrant

      # Kubernetes and Docker utils
      pkgs.docker-compose
      pkgs.kubectl
      pkgs.kubectx
      pkgs.kubetail
      pkgs.kubent
      pkgs.kubernetes-helm
      pkgs.minikube
      # pkgs.octant

      # Misc
      pkgs.mariadb
      pkgs.awscli2
      pkgs.ssm-session-manager-plugin # Session manager plugin for aws
      pkgs.terraform
      pkgs.terragrunt
    ];

    # Enable appgate
    programs.appgate-sdp.enable = true;

    # Enable orca the screen reader
    services.orca.enable = true;
  };
}
