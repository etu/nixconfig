{ ... }:

{
  environment.persistence."/persistent" = {
    directories = [
      "/etc/nixos"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];
  };

  # Bind mount for persistent certificates for nginx
  fileSystems."/var/lib/acme" = {
    device = "/persistent/var/lib/acme";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  # Persistance of concates home directory
  fileSystems."/home/concate" = {
    device = "/persistent/home/concate";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  # Persist root's .ssh directory
  fileSystems."/root/.ssh" = {
    device = "/persistent/home/root/.ssh";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  my.user.persistent = {
    extraFiles = [
      ".config/fish/fish_variables"
    ];
    extraDirectories = [
      ".dotfiles"
      ".local/share/direnv"
      ".local/share/emacs"
      ".local/share/fish"
      ".ssh"
      "backups"
      "code"
      "documents"
      "org"
    ];
  };
}
