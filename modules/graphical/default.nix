{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./fdm-printing
    ./firefox
    ./flatpak
    ./gnupg
    ./hamradio
    ./signal
    ./sway
    ./telegram
    ./terminal
    ./theme
    ./virtualbox
    ./window-managers
    ./xkb-keymap
  ];

  options.etu.graphical.enable = lib.mkEnableOption "Enable graphical settings";

  config = lib.mkIf config.etu.graphical.enable {
    etu = {
      graphical.firefox.enable = lib.mkDefault true;
      graphical.flatpak.enable = lib.mkDefault true;
      graphical.gnupg.enable = lib.mkDefault true;
      graphical.telegram.enable = lib.mkDefault true;
      graphical.terminal.enable = lib.mkDefault true;
      graphical.theme.enable = lib.mkDefault true;

      # Define extra groups for user.
      user.extraGroups = ["networkmanager" "adbusers"];

      # Install using home-manager.
      user.extraUserPackages = [
        pkgs.chromium # Chromium browser
        pkgs.delfin # Jellyfin client
        pkgs.feh # Image display tool
        pkgs.imv # Image display tool
        pkgs.mpv # Media player
        pkgs.pavucontrol # Pulse audio volume control
        pkgs.sshfs-fuse # SSHFS client
        pkgs.stupidterm # Another terminal emulator
        pkgs.yt-dlp # Youtube download client
      ];

      # Directories to mount persistent for my user on graphical sessions
      base.zfs.user.directories = [
        ".config/delfin"
        ".config/pipewire/media-session.d"
        ".local/state/wireplumber"
        "Downloads"
        "code"
        "documents"
        "org"

        # Persist chromium config directory
        ".config/chromium"

        # Persist gnome online accounts and gnome keyrings directories.
        ".config/goa-1.0"
        ".local/share/keyrings"
      ];

      # Persistence of certain hosts paths for graphical systems.
      base.zfs.system.directories = [
        "/etc/nixos"
        "/etc/NetworkManager/system-connections"
        "/var/lib/bluetooth"
        "/var/lib/iwd"
      ];
    };

    # Mount the /etc/nixos directory in the home directory as well.
    fileSystems."/home/${config.etu.user.username}/code/nixos" = {
      device = "${config.etu.dataPrefix}/etc/nixos";
      options = ["bind" "noauto" "x-systemd.automount" "x-systemd.requires-mounts-for=${config.etu.dataPrefix}"];
    };

    # Install adb and fastboot.
    programs.adb.enable = true;

    # Enable gnome keyring (related to ~/.config/goa-1.0 and ~/.local/share/keyrings).
    services.gnome.gnome-keyring.enable = true;

    # Enable networkmanager.
    networking.networkmanager.enable = true;
    networking.networkmanager.wifi.backend = "iwd";
    networking.wireless.iwd.settings.Settings.AutoConnect = true;

    # 8000 is for random web sharing things.
    networking.firewall.allowedTCPPorts = [8000];

    # Install some comand line tools I cummonly want available for my
    # home directory on graphical systems.
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.file = {
        # Mpv config file - Don't show images embedded in music files
        ".config/mpv/mpv.conf".text = "no-audio-display";

        ".XCompose".text = ''
          include "%L"

          # Default already
          # <Multi_key> <a> <a>: "å"
          # <Multi_key> <A> <A>: "Å"

          # Some nice binds
          <Multi_key> <a> <e>: "ä"
          <Multi_key> <A> <E>: "Ä"
          <Multi_key> <o> <e>: "ö"
          <Multi_key> <O> <E>: "Ö"

          # Table flip multi key
          <Multi_key> <t> <f>: "(ノಠ益ಠ)ノ彡┻━┻"

          # Shruggie
          <Multi_key> <s> <h>: "¯\\_(ツ)_/¯"
        '';
      };
    };
  };
}
