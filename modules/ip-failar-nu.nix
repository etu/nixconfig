{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.ip-failar-nu;

  package = pkgs.buildGoPackage {
    pname = "ip-failar-nu";
    version = "20180318";

    goPackagePath = "github.com/etu/ip.failar.nu";
    goDeps = ./ip-failar-nu-deps.nix;

    src = pkgs.fetchFromGitHub {
      owner = "etu";
      repo = "ip.failar.nu";
      rev = "c98fe6421ed79ee78a370fb067445f2865258560";
      sha256 = "0pddnswcswkf8z7c6ajhf2ps2n6kfhrgnx6kcph7jz81n5488vyz";
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
        ExecStart = "${package}/bin/ip.failar.nu";
        Restart = "always";
      };
    };
  };
}
