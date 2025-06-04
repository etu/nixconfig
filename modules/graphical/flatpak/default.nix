{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.flatpak.enable = lib.mkEnableOption "Enable graphical flatpak settings";
  options.etu.graphical.flatpak.enablePersistence = lib.mkEnableOption "Enable graphical flatpak persistence settings";

  config = lib.mkIf config.etu.graphical.flatpak.enable {
    # Install appcenter
    etu.user.extraUserPackages = [
      pkgs.pantheon.appcenter
    ];

    # Required for link opening to browsers to work in apps when on Sway
    xdg.portal.extraPortals = with pkgs; [xdg-desktop-portal-gtk];

    # Mount flatpak persistence under flatpak data
    etu.base.zfs.local.directories = lib.mkIf config.etu.graphical.flatpak.enablePersistence [
      "/var/lib/flatpak"
    ];

    etu.base.zfs.localUser.directories = lib.mkIf config.etu.graphical.flatpak.enablePersistence [
      ".cache/io.elementary.appcenter/"
      ".local/share/flatpak/"
      ".var/app/"
    ];

    # Hack to make bind mount to allow exec since I don't allow it on
    # the parent filesystem. Otherwise it doesn't really work to
    # install or run things :'D
    fileSystems = lib.mkIf config.etu.graphical.flatpak.enablePersistence {
      "/var/lib/flatpak".options = ["exec"];
    };

    # Enable flatpak
    services.flatpak.enable = true;

    # Add the global override to allow reading the ~/.XCompose file in
    # all the flatpak applications.
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.file.".local/share/flatpak/overrides/com.slack.Slack".text = ''
        [Context]
        filesystems=~/.XCompose:ro
      '';
      home.file.".local/share/flatpak/overrides/com.discordapp.Discord".text = ''
        [Context]
        filesystems=~/.XCompose:ro
      '';
      home.file.".local/share/flatpak/overrides/dev.vencord.Vesktop".text = ''
        [Context]
        filesystems=~/.XCompose:ro
      '';
      home.file.".local/share/flatpak/overrides/net.lutris.Lutris".text = ''
        [Context]
        filesystems=/data/local/home/${config.etu.user.username}/Games:rw
      '';
    };

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
