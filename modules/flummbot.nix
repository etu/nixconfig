{ config, lib, inputs, pkgs, ... }:
let
  cfg = config.programs.flummbot;

  package = inputs.flummbot.defaultPackage.x86_64-linux;
in
{
  options.programs.flummbot = {
    enable = lib.mkEnableOption "Small IRC bot in go used for my channels";

    user = lib.mkOption {
      type = lib.types.str;
      default = "bots";
      defaultText = "bots";
      description = "Username, used for storage of config file ~/flummbot.toml";
    };

    stateDirectory = lib.mkOption {
      type = lib.types.str;
      default = "/home/bots";
      defaultText = "/home/bots";
      description = "Directory where configs and database is stored";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.flummbot = {
      description = "flummbot";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ package ];
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        ExecStart = "${package}/bin/flummbot --config ${cfg.stateDirectory}/flummbot.toml";
        WorkingDirectory = cfg.stateDirectory;
        Restart = "always";
      };
    };
  };
}
