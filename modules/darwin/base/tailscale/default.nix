{
  config,
  lib,
  ...
}:
{
  options.etu.base.tailscale.enable = lib.mkEnableOption "Enable tailscale settings";

  config = lib.mkIf config.etu.base.tailscale.enable {
    # Tailscale, running the open source tailscaled as a launchd daemon (no
    # menu bar app). Log in once after activation with:
    # $ sudo tailscale up
    services.tailscale.enable = true;
  };
}
