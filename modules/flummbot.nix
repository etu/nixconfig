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
      rev = "f27d5c89cb79b10979d9d8ca237bacba8e4d673d";
      sha256 = "0gpkjqrjvmi90jvqihxzx2j06kknga8xybf71lmj9h7l768mly0f";
    };
  };

in {
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
