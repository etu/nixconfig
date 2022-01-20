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
      files = [
        ".config/fish/fish_variables"
      ];
      directories = [
        ".dotfiles"
        ".ssh"
        ".weechat"
      ];
    };
  };

  # Persistence of all users dotfiles between boots
  fileSystems."/home/bots" = {
    device = "/persistent/home/bots";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  fileSystems."/home/concate" = {
    device = "/persistent/home/concate";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  fileSystems."/home/talyz" = {
    device = "/persistent/home/talyz";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  fileSystems."/home/ozeloten" = {
    device = "/persistent/home/ozeloten";
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
