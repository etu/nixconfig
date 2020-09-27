{ config, lib, pkgs, ... }:
let
  cfg = config.programs.flummbot;

  package = pkgs.buildGoPackage {
    pname = "flummbot";
    version = "20200927";

    goPackagePath = "github.com/etu/flummbot";
    goDeps = ./flummbot-deps.nix;

    src = pkgs.fetchFromGitHub {
      owner = "etu";
      repo = "flummbot";
      rev = "484056f5b392fa4416a7b899f11dfa73181663f9";
      sha256 = "1kqkmmbc89fn9xz6fwd3dn6lx0lyrq9sz1bi8kkkd1w5ikcxzmlb";
    };
  };

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
        ExecStart = "${package}/bin/flummbot -c ${cfg.stateDirectory}/flummbot.toml";
        WorkingDirectory = cfg.stateDirectory;
        Restart = "always";
      };
    };
  };
}
