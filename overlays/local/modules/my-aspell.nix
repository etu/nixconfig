{ config, lib, pkgs, ... }:

with lib;

let
 cfg = config.my.aspell;

in {
  options = {
    my.aspell = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable and install aspell with swedish and english dictionary
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      aspell

      # Dictionaries
      aspellDicts.en aspellDicts.en-computers aspellDicts.en-science
      aspellDicts.sv
    ];

    # Configure aspell system wide
    environment.etc."aspell.conf".text = ''
      master en_US
      extra-dicts en-computers.rws
      add-extra-dicts en_US-science.rws sv.rws
    '';
  };
}
