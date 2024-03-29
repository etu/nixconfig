{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.development.direnv.enable = lib.mkEnableOption "Enable development direnv settings";

  config = lib.mkIf config.etu.development.direnv.enable {
    # Enable lorri on all systems.
    services.lorri.enable = true;

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.file = {
        # Configure lorri to be backed by direnv
        ".direnvrc".text = ''
          use_nix() {
            eval "$(lorri direnv)"
          }
        '';
      };
    };

    # Install direnv using home manager.
    etu.user.extraUserPackages = [
      pkgs.direnv
    ];

    # Enable persistence for direnv files.
    etu.base.zfs.user.directories = [
      ".local/share/direnv"
    ];
  };
}
