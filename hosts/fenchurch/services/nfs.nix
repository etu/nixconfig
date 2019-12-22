{ config, ... }:

{
  # NFS Server
  services.nfs.server.enable = true;
  services.nfs.server.exports = ''
    /media/legacy/files        10.3.0.0/24(ro,sync,no_subtree_check,fsid=0)
  '';

  networking.firewall.allowedTCPPorts = [ 111 2049 20048 ];
  networking.firewall.allowedUDPPorts = [ 111 2049 20048 ];
}
