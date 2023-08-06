{
  config,
  lib,
  ...
}: {
  options.etu.base.tailscale.enable = lib.mkEnableOption "Enable tailscale settings";

  config = lib.mkIf config.etu.base.tailscale.enable {
    services.tailscale.enable = true;

    # Always allow traffic from your Tailscale network
    networking.firewall.trustedInterfaces = ["tailscale0"];

    # Persistence of state
    etu.base.zfs.system.directories = [
      "/var/lib/tailscale"
    ];
  };
}
