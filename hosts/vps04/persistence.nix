{ ... }:

{
  environment.persistence."/persistent".files = [
    "/etc/machine-id"
    "/etc/ssh/ssh_host_rsa_key"
    "/etc/ssh/ssh_host_rsa_key.pub"
    "/etc/ssh/ssh_host_ed25519_key"
    "/etc/ssh/ssh_host_ed25519_key.pub"
  ];

  # Persistence of all users dotfiles between boots
  fileSystems."/home/bots" = {
    device = "/persistent/home/bots";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  fileSystems."/home/concate" = {
    device = "/persistent/home/concate";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  fileSystems."/home/talyz" = {
    device = "/persistent/home/talyz";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  fileSystems."/home/ozeloten" = {
    device = "/persistent/home/ozeloten";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  # Persistence of roots dotfiles between boots
  fileSystems."/root" = {
    device = "/persistent/home/root";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };
}
