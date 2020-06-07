{ config, lib, pkgs, ... }:

let
  cfg = config.my.vbox;

in
{
  options.my.vbox.enable = lib.mkEnableOption "Enables vbox and related things I use.";

  config = lib.mkIf cfg.enable {
    # Enable virtualbox.
    virtualisation.virtualbox.host.enable = true;

    # Add user to group
    my.user.extraGroups = [ "vboxusers" ];
  };
}
