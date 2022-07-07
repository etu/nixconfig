{ config, lib, pkgs, ... }:
let
  cfg = config.my.common-cli;

in
{
  config = lib.mkIf cfg.enable {
    # Enable the OpenSSH daemon.
    services.openssh.enable = true;
    services.openssh.passwordAuthentication = false;

    # Enable mosh.
    programs.mosh.enable = true;

    # Enable firewall.
    networking.firewall.enable = true;
    networking.firewall.allowPing = true;

    # Enable doas on all systems.
    security.doas.enable = true;
  };
}
