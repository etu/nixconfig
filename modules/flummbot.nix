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
      rev = "ac418a21e37dc13e6bed716eb5f75ce179696910";
      sha256 = "1z72na4pffn4lgs23jvway5k7ggmlnc0qhwcny20d6y84qchgfzm";
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
        ExecStart = "${package}/bin/flummbot --config ${cfg.stateDirectory}/flummbot.toml";
        WorkingDirectory = cfg.stateDirectory;
        Restart = "always";
      };
    };
  };
}
