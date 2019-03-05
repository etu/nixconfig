{ config, lib, pkgs, ... }:

with lib;

let
 cfg = config.my.vbox;

in {
  options = {
    my.vbox = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enables vbox and related things I use.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (vagrant.override (oldAttrs: rec {
        withLibvirt = false;
      }))
    ];

    # Enable virtualbox.
    virtualisation.virtualbox.host.enable = true;

    # Add user to group
    my.user.extraGroups = [ "vboxusers" ];
  };
}
