{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.ip-failar-nu;

  package = pkgs.stdenv.mkDerivation {
    pname = "ip-failar-nu";
    version = "20180318";

    nativeBuildInputs = [ pkgs.go ];
    installPhase = "install -D ip-failar-nu $out/bin/ip-failar-nu";

    src = pkgs.fetchFromGitHub {
      owner = "etu";
      repo = "ip.failar.nu";
      rev = "925218c6615659e56faabbab64146dff8c38b55c";
      sha256 = "0qmvya8ilgj3y38dxy7qk64cxpfjrbp78iihj8nl97iqq29s5lf0";
      fetchSubmodules = true;
    };
  };

in {
  options.programs.ip-failar-nu = {
    enable = mkEnableOption "A service that responds over http with the connecting clients IP.";
  };

  config = mkIf cfg.enable {
    systemd.services.ip-failar-nu = {
      description = "ip-failar-nu";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ package ];
      serviceConfig = {
        Type = "simple";
        User = "nobody";
        ExecStart = "${package}/bin/ip-failar-nu";
        Restart = "always";
      };
    };
  };
}
