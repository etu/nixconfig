{ ... }:

{
  environment.etc."ssh/ssh_host_rsa_key".source = "/nix/persistent/etc/ssh/ssh_host_rsa_key";
  environment.etc."ssh/ssh_host_rsa_key.pub".source = "/nix/persistent/etc/ssh/ssh_host_rsa_key.pub";
  environment.etc."ssh/ssh_host_ed25519_key".source = "/nix/persistent/etc/ssh/ssh_host_ed25519_key";
  environment.etc."ssh/ssh_host_ed25519_key.pub".source = "/nix/persistent/etc/ssh/ssh_host_ed25519_key.pub";

  # Avoid the need to have a moved config and help muscle memory of location
  fileSystems."/etc/nixos" = {
    device = "/nix/persistent/etc/nixos";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  # Persistence of gitea and postgresql data files between boots
  fileSystems."/var/lib/gitea" = {
    device = "/nix/persistent/var/lib/gitea";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };
  fileSystems."/var/lib/postgresql" = {
    device = "/nix/persistent/var/lib/postgresql";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  # Persistence of caddy certificaties between boots
  fileSystems."/var/lib/caddy/acme" = {
    device = "/nix/persistent/var/lib/caddy/acme";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  # Persistence of logs between boots
  fileSystems."/var/log" = {
    device = "/nix/persistent/var/log";
    options = [ "bind" ];
  };

  # Persistence of roots dotfiles between boots
  fileSystems."/root" = {
    device = "/nix/persistent/home/root";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };
}
