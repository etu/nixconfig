{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.flummbot;
  flummbot = cfg.package;

in {
  options = {
    programs.flummbot = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Small IRC bot in go used for my channels
        '';
      };

      package = mkOption {
        type = types.package;
        default = pkgs.flummbot;
        defaultText = "pkgs.flummbot";
        description = "flummbot derivation to use";
      };

      user = mkOption {
        type = types.string;
        default = "bots";
        defaultText = "bots";
        description = "Username, used for storage of config file ~/flummbot.toml";
      };

      stateDirectory = mkOption {
        type = types.string;
        default = "/home/bots";
        defaultText = "/home/bots";
        description = "Directory where configs and database is stored";
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.flummbot = {
      description = "flummbot";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ cfg.package ];
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        ExecStart = "${cfg.package}/bin/flummbot -c ${cfg.stateDirectory}/flummbot.toml";
        WorkingDirectory = cfg.stateDirectory;
        Restart = "always";
      };
    };
  };
}
