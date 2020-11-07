{ config, lib, inputs, pkgs, ... }:
let
  cfg = config.programs.ip-failar-nu;

  package = inputs.ip-failar-nu.defaultPackage.x86_64-linux;

in
{
  options.programs.ip-failar-nu = {
    enable = lib.mkEnableOption "A service that responds over http with the connecting clients IP.";
  };

  config = lib.mkIf cfg.enable {
    systemd.services.ip-failar-nu = {
      description = "ip-failar-nu";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ package ];
      serviceConfig = {
        Type = "simple";
        User = "nobody";
        ExecStart = "${package}/bin/ip.failar.nu";
        Restart = "always";
      };
    };
  };
}
