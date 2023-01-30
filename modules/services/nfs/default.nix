{
  config,
  lib,
  ...
}: {
  options.etu.services.nfs = {
    enable = lib.mkEnableOption "Enable services nfs settings";
    exports = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Exports for nfs";
    };
  };

  config = lib.mkIf config.etu.services.nfs.enable {
    # Enable nfs server.
    services.nfs.server.enable = true;

    # Configure ports
    services.nfs.server.statdPort = 4000;
    services.nfs.server.lockdPort = 4001;

    # Configure NFSd to listen to UDP
    services.nfs.server.extraNfsdConfig = "udp=y";

    # Open ports used by NFS
    networking.firewall.allowedTCPPorts = [2049 111 4000 4001 20048];
    networking.firewall.allowedUDPPorts = [2049 111 4000 4001 20048];

    services.nfs.server.exports = config.etu.services.nfs.exports;
  };
}
