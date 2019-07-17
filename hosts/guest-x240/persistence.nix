{ ... }:

{
  environment.etc."machine-id".source = "/nix/persistent/etc/machine-id";

  # Avoid the need to have a moved config and help muscle memory of location
  fileSystems."/etc/nixos" = {
    device = "/nix/persistent/etc/nixos";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  # Persistence of NetworkManager network connections.
  fileSystems."/etc/NetworkManager/system-connections/" = {
    device = "/nix/persistent/etc/NetworkManager/system-connections/";
    options = [ "ro" "bind" "noauto" "x-systemd.automount" ];
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
