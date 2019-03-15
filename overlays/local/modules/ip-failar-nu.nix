{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.ip-failar-nu;
  ip-failar-nu = cfg.package;

in {
  options.programs.ip-failar-nu = {
    enable = mkEnableOption "A service that responds over http with the connecting clients IP.";
    package = mkOption {
      type = types.package;
      default = pkgs.ip-failar-nu;
      defaultText = "pkgs.ip-failar-nu";
      description = "ip-failar-nu derivation to use";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.ip-failar-nu = {
      description = "ip-failar-nu";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ cfg.package ];
      serviceConfig = {
        Type = "simple";
        User = "nobody";
        ExecStart = "${cfg.package}/bin/ip-failar-nu";
        Restart = "always";
      };
    };
  };
}
