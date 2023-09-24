{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.flatpak.enable = lib.mkEnableOption "Enable graphical flatpak settings";
  options.etu.graphical.flatpak.enablePersistence = lib.mkEnableOption "Enable graphical flatpak persistence settings";

  config = lib.mkIf config.etu.graphical.flatpak.enable {
    etu.user.extraUserPackages = [
      pkgs.pantheon.appcenter
    ];

    etu.base.zfs.system.directories = lib.mkIf config.etu.graphical.flatpak.enablePersistence [
      "/var/lib/flatpak"
    ];

    # Hack to make bind mount to allow exec since I don't allow it on
    # the parent filesystem. Otherwise it doesn't really work to
    # install or run things :'D
    fileSystems = lib.mkIf config.etu.graphical.flatpak.enablePersistence {
      "/var/lib/flatpak".options = ["exec"];
    };

    etu.base.zfs.user.directories = lib.mkIf config.etu.graphical.flatpak.enablePersistence [
      ".cache/io.elementary.appcenter/"
      ".local/share/flatpak/"
      ".var/app/"
    ];

    services.flatpak.enable = true;

    systemd.services.flatpak-setup-flathub = {
      wantedBy = ["multi-user.target"];
      path = [pkgs.flatpak];
      script = ''
        flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
      '';
    };
  };
}
