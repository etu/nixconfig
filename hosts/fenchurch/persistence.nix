{ config, ... }:

{
  environment.persistence."/persistent" = {
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];
    users.${config.my.user.username} = {
      files = [];
      directories = [
        ".config/fish"
        ".dotfiles"
        ".local/share/fish"
        ".ssh"
        "backups"
      ];
    };
  };

  # Bind mount for persistent certificates for nginx
  fileSystems."/var/lib/acme" = {
    device = "/persistent/var/lib/acme";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };
}
