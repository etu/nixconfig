{ config, ... }:
{
  etu = {
    # Directories to mount persistent for my user on graphical sessions
    base.zfs.user.directories = [
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
    options = [
      "bind"
      "noauto"
      "x-systemd.automount"
      "x-systemd.requires-mounts-for=${config.etu.dataPrefix}"
    ];
  };
}
