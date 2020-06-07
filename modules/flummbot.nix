{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.flummbot;

  package = pkgs.buildGoPackage {
    pname = "flummbot";
    version = "20180703";

    goPackagePath = "github.com/etu/flummbot";
    goDeps = ./flummbot-deps.nix;

    src = pkgs.fetchFromGitHub {
      owner = "etu";
      repo = "flummbot";
      rev = "b1f72875ad5311cf7e97fc301a0c40bf9be008bc";
      sha256 = "16bb6b71i873g3ffmn9jwygkamc3p7j4lnivkxaclik1ivb54xgk";
    };
  };

in
{
  options.programs.flummbot = {
    enable = mkEnableOption "Small IRC bot in go used for my channels";

    user = mkOption {
      type = types.str;
      default = "bots";
      defaultText = "bots";
      description = "Username, used for storage of config file ~/flummbot.toml";
    };

    stateDirectory = mkOption {
      type = types.str;
      default = "/home/bots";
      defaultText = "/home/bots";
      description = "Directory where configs and database is stored";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.flummbot = {
      description = "flummbot";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ package ];
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        ExecStart = "${package}/bin/flummbot -c ${cfg.stateDirectory}/flummbot.toml";
        WorkingDirectory = cfg.stateDirectory;
        Restart = "always";
      };
    };
  };
}
