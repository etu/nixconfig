{ config, lib, pkgs, ... }:
let
  cfg = config.my.vbox;

in
{
  config = lib.mkIf cfg.enable {
    # Enable virtualbox.
    virtualisation.virtualbox.host.enable = true;

    # Add user to group
    etu.user.extraGroups = [ "vboxusers" ];
  };
}
