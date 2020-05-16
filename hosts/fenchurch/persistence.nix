{ ... }:

{
  environment.etc."machine-id".source = "/persistent/etc/machine-id";

  environment.etc."ssh/ssh_host_rsa_key".source = "/persistent/etc/ssh/ssh_host_rsa_key";
  environment.etc."ssh/ssh_host_rsa_key.pub".source = "/persistent/etc/ssh/ssh_host_rsa_key.pub";
  environment.etc."ssh/ssh_host_ed25519_key".source = "/persistent/etc/ssh/ssh_host_ed25519_key";
  environment.etc."ssh/ssh_host_ed25519_key.pub".source = "/persistent/etc/ssh/ssh_host_ed25519_key.pub";

  # Avoid the need to have a moved config and help muscle memory of location
  environment.etc."nixos".source = "/persistent/etc/nixos";

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
