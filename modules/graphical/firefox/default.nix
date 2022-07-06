{ config, lib, pkgs, ... }:

{
  options.etu.graphical.firefox = {
    enable = lib.mkEnableOption "Enable graphical firefox settings";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.firefox-bin;
      description = "Firefox package to use.";
      readOnly = true;
    };
  };

  config = lib.mkIf config.etu.graphical.firefox.enable {
    # Configure firefox for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Enable browserpass integration
      programs.browserpass.enable = true;
      programs.browserpass.browsers = [ "firefox" ];

      # Make firefox the default browser
      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "text/html" = [ "firefox.desktop" ];
          "x-scheme-handler/http" = [ "firefox.desktop" ];
          "x-scheme-handler/https" = [ "firefox.desktop" ];
          "x-scheme-handler/about" = [ "firefox.desktop" ];
          "x-scheme-handler/unknown" = [ "firefox.desktop" ];
        };
      }; # END xdg.mimeApps

      # Install my defined firefox package
      programs.firefox = {
        enable = true;
        package = config.etu.graphical.firefox.package;
      };
    };

    # Enable persistence for firefox files.
    environment.persistence."/persistent" = {
      users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
        directories = [
          ".mozilla/firefox"
        ];
      };
    };
  };
}
