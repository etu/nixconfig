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
