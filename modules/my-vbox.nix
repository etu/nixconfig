{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.vbox;

in {
  options.my.vbox.enable = mkEnableOption "Enables vbox and related things I use.";

  config = mkIf cfg.enable {
    # Enable virtualbox.
    virtualisation.virtualbox.host.enable = true;

    # Add user to group
    my.user.extraGroups = [ "vboxusers" ];
  };
}
