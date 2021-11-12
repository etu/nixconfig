{ ... }:

{
  environment.persistence."/persistent".files = [
    "/etc/machine-id"
    "/etc/ssh/ssh_host_rsa_key"
    "/etc/ssh/ssh_host_rsa_key.pub"
    "/etc/ssh/ssh_host_ed25519_key"
    "/etc/ssh/ssh_host_ed25519_key.pub"
  ];

  # Persistence of gitea and postgresql data files between boots
  fileSystems."/var/lib/gitea" = {
    device = "/persistent/var/lib/gitea";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };
  fileSystems."/var/lib/postgresql" = {
    device = "/persistent/var/lib/postgresql";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Bind mount for persistent certificates for nginx
  fileSystems."/var/lib/acme" = {
    device = "/persistent/var/lib/acme";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Persistence of roots dotfiles between boots
  fileSystems."/root" = {
    device = "/persistent/home/root";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };
}
