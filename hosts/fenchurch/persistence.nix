{ ... }:

{
  environment.persistence."/persistent".files = [
    "/etc/machine-id"
    "/etc/ssh/ssh_host_rsa_key"
    "/etc/ssh/ssh_host_rsa_key.pub"
    "/etc/ssh/ssh_host_ed25519_key"
    "/etc/ssh/ssh_host_ed25519_key.pub"
  ];

  # Bind mount for persistent certificates for nginx
  fileSystems."/var/lib/acme" = {
    device = "/persistent/var/lib/acme";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  my.user.persistent = {
    extraFiles = [
      ".config/fish/fish_variables"
    ];
    extraDirectories = [
      ".dotfiles"
      ".local/share/fish"
      ".ssh"
      "backups"
    ];
  };
}
