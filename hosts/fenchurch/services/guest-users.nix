{ config, pkgs, ... }:

let
  # Import my ssh public keys
  keys = import ../../../data/pubkeys.nix;

  # home / chroot path
  path = "/home/guests";
in {
  users.users.guests = {
    isNormalUser = true;
    uid = 1002;
    home = path;
    openssh.authorizedKeys.keys = keys.talyz ++ keys.guests;
  };

  services.openssh.extraConfig = ''
    Match User guests
      ChrootDirectory ${path}
      AllowTCPForwarding no
      X11Forwarding no
      ForceCommand internal-sftp
  '';

  fileSystems."${path}/files" = {
    device = "/media/legacy/files";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };
}
