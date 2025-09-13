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

    # Use systemd-resolved for DNS resolution to not rely on
    # tailscaled. It seems like tailscaled has troubles when it comes
    # to suspend and resume and the network not being available right
    # after resume.
    services.resolved.enable = true;
  };
}
