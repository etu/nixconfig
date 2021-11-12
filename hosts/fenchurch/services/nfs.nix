{ config, ... }:

{
  # NFS Server
  services.nfs.server.enable = true;
  services.nfs.server.exports = ''
    /media/zstorage/files          192.168.0.0/24(ro,no_subtree_check,crossmnt)
    /media/zstorage/files/audio    192.168.0.0/24(ro,no_subtree_check)
    /media/zstorage/files/ebooks   192.168.0.0/24(ro,no_subtree_check)
    /media/zstorage/files/software 192.168.0.0/24(ro,no_subtree_check)
    /media/zstorage/files/upload   192.168.0.0/24(ro,no_subtree_check)
    /media/zstorage/files/video    192.168.0.0/24(ro,no_subtree_check)
  '';

  networking.firewall.allowedTCPPorts = [ 111 2049 20048 ];
  networking.firewall.allowedUDPPorts = [ 111 2049 20048 ];
}
