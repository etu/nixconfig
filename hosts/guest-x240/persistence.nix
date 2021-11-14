{ ... }:

{
  environment.etc."machine-id".source = "/persistent/etc/machine-id";

  # Avoid the need to have a moved config and help muscle memory of location
  fileSystems."/etc/nixos" = {
    device = "/persistent/etc/nixos";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Persistence of NetworkManager network connections.
  fileSystems."/etc/NetworkManager/system-connections" = {
    device = "/persistent/etc/NetworkManager/system-connections/";
    options = [ "ro" "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Persistence of roots dotfiles between boots
  fileSystems."/root" = {
    device = "/persistent/home/root";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };
}
