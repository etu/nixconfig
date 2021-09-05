{ config, pkgs, ... }:
let
  # Import my ssh public keys
  keys = import ../../../data/pubkeys.nix;

  # home / chroot path
  path = "/home/guests";
in
{
  users.users.guests = {
    isNormalUser = true;
    createHome = false;
    uid = 1002;
    home = path;
    openssh.authorizedKeys.keys = keys.guests;
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
    options = [ "ro" "bind" ];
    depends = [ "/media/legacy/files" ];
  };

  fileSystems."${path}/files/upload" = {
    device = "/media/legacy/files/upload";
    options = [ "rw" "bind" ];
    depends = [ "/media/legacy/files" "${path}/files" ];
  };
}
