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

  # Read only filesystems
  fileSystems."${path}/files" = {
    device = "/media/zstorage/files";
    options = [ "ro" "bind" ];
    depends = [ "/media/zstorage/files" ];
    noCheck = true;
  };

  fileSystems."${path}/files/audio" = {
    device = "/media/zstorage/files/audio";
    options = [ "ro" "bind" ];
    depends = [ "/media/zstorage/files" "${path}/files" ];
    noCheck = true;
  };

  fileSystems."${path}/files/ebooks" = {
    device = "/media/zstorage/files/ebooks";
    options = [ "ro" "bind" ];
    depends = [ "/media/zstorage/files" "${path}/files" ];
    noCheck = true;
  };

  fileSystems."${path}/files/software" = {
    device = "/media/zstorage/files/software";
    options = [ "ro" "bind" ];
    depends = [ "/media/zstorage/files" "${path}/files" ];
    noCheck = true;
  };

  fileSystems."${path}/files/video" = {
    device = "/media/zstorage/files/video";
    options = [ "ro" "bind" ];
    depends = [ "/media/zstorage/files" "${path}/files" ];
    noCheck = true;
  };

  # Read write upload directory
  fileSystems."${path}/files/upload" = {
    device = "/media/zstorage/files/upload";
    options = [ "rw" "bind" ];
    depends = [ "/media/zstorage/files" "${path}/files" ];
    noCheck = true;
  };
}
