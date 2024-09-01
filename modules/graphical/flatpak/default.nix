{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.flatpak.enable = lib.mkEnableOption "Enable graphical flatpak settings";
  options.etu.graphical.flatpak.enablePersistence = lib.mkEnableOption "Enable graphical flatpak persistence settings";
  options.etu.graphical.flatpak.persistencePath = lib.mkOption {
    type = lib.types.str;
    default = config.etu.dataPrefix;
    description = "Data prefix for persistent state of flatpak data.";
  };

  config = lib.mkIf config.etu.graphical.flatpak.enable {
    # Install appcenter
    etu.user.extraUserPackages = [
      pkgs.pantheon.appcenter
    ];

    # Required for link opening to browsers to work in apps when on Sway
    xdg.portal.extraPortals = with pkgs; [xdg-desktop-portal-gtk];

    # Mount flatpak persistence under flatpak data
    environment.persistence."${config.etu.graphical.flatpak.persistencePath}" = lib.mkIf config.etu.graphical.flatpak.enablePersistence {
      # Global state for flatpak
      directories = [
        "/var/lib/flatpak"
      ];
      # User state for flatpak and appcenter
      users.${config.etu.user.username}.directories = [
        ".cache/io.elementary.appcenter/"
        ".local/share/flatpak/"
        ".var/app/"
      ];
    };

    # Hack to make bind mount to allow exec since I don't allow it on
    # the parent filesystem. Otherwise it doesn't really work to
    # install or run things :'D
    fileSystems = lib.mkIf config.etu.graphical.flatpak.enablePersistence {
      "/var/lib/flatpak".options = ["exec"];
    };

    # Enable flatpak
    services.flatpak.enable = true;

    # Configure flathub
    systemd.services.flatpak-setup-flathub = {
      wantedBy = ["multi-user.target"];
      path = [pkgs.flatpak];
      script = ''
        flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
      '';
    };
  };
}
