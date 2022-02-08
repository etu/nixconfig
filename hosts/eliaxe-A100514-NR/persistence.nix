{ config, ... }:

{
  environment.persistence."/persistent" = {
    directories = [
      "/etc/nixos"
      "/etc/NetworkManager/system-connections"
      "/var/lib/bluetooth"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];
    users.${config.my.user.username} = {
      files = [
        ".caffrc"
        ".config/fish/fish_variables"
        ".msmtprc"

        # Spotify
        ".config/spotify/prefs"

        # Work
        ".docker/config.json"
        ".kube/config"
        "bin/gh-md-toc"
        "bin/git-tvlog"
        "bin/mydump"
      ];
      directories = [
        ".config/pipewire/media-session.d"
        ".config/syncthing"
        ".config/tvnu"
        ".dotfiles"
        ".gnupg"
        ".local/share/TelegramDesktop/tdata"
        ".local/share/dino"
        ".local/share/direnv"
        ".local/share/emacs"
        ".local/share/fish"
        ".mozilla/firefox"
        ".password-store"
        ".ssh"
        "Downloads"
        "code"
        "documents"
        "org"

        # Spotify
        ".config/spotify/Users"

        # Work
        ".chalet"
        ".chef"
        ".config/Slack"
        ".config/VirtualBox"
        ".vagrant.d"
        "VirtualBox VMs"
        "tvnu"

        # Evolution
        ".config/evolution"
        ".config/goa-1.0"
        ".local/share/evolution"
        ".local/share/keyrings"
      ];
    };
  };
}
