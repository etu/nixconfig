{ config, lib, pkgs, ... }:

with lib;

let
 cfg = config.my.nfsd;

in {
  options = {
    my.dmrconfig = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable dmrconfig and add udev rules.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # Enable udev rules for steam controller
    services.udev.extraRules = ''
      # Baofeng RD-5R, TD-5R
      SUBSYSTEM=="usb", ATTRS{idVendor}=="15a2", ATTRS{idProduct}=="0073", MODE="666"
    '';

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      dmrconfig
    ];
  };
}
