{ config, lib, pkgs, ... }:

{
  options.etu.graphical.evolution.enable = lib.mkEnableOption "Enable graphical evolution settings";

  config = lib.mkIf config.etu.graphical.evolution.enable {
    # Set up services needed for gnome stuff for evolution.
    services.gnome.evolution-data-server.enable = true;
    services.gnome.gnome-keyring.enable = true;

    # Configure evolution for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [
        pkgs.evolution
      ];

      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
        };
        associations.added = {
          "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
        };
      }; # END xdg.mimeApps
    };

    # Enable persistence for evolution files.
    environment.persistence."/persistent" = {
      users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
        directories = [
          ".config/evolution"
          ".config/goa-1.0"
          ".local/share/evolution"
          ".local/share/keyrings"
        ];
      };
    };
  };
}
