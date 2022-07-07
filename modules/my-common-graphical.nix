{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.etu.graphical.enable {
    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      dino

      chromium

      stupidterm

      pulseeffects-pw
    ];

    # Enable networkmanager.
    networking.networkmanager.enable = true;
    networking.networkmanager.wifi.backend = "iwd";

    # 8000 is for random web sharing things.
    networking.firewall.allowedTCPPorts = [ 8000 ];

    # Define extra groups for user.
    etu.user.extraGroups = [ "networkmanager" "dialout" ];
  };
}
