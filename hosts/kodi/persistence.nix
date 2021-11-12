{ ... }:

{
  environment.persistence."/persistent".files = [
    "/etc/machine-id"
    "/etc/ssh/ssh_host_rsa_key"
    "/etc/ssh/ssh_host_rsa_key.pub"
    "/etc/ssh/ssh_host_ed25519_key"
    "/etc/ssh/ssh_host_ed25519_key.pub"
  ];

  # Persistence of roots dotfiles between boots
  fileSystems."/root" = {
    device = "/home/root";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };
}
