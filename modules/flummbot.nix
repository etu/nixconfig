{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.flummbot;

  package = pkgs.stdenv.mkDerivation {
    pname = "flummbot";
    version = "20180703";

    nativeBuildInputs = [ pkgs.go ];
    installPhase = "install -D flummbot $out/bin/flummbot";

    src = pkgs.fetchFromGitHub {
      owner = "etu";
      repo = "flummbot";
      rev = "b32f569b6040ebdc7b258c95be4e17e41a152d59";
      sha256 = "0hw8dprji4l34dcpwhcq4nmnqwwbv33ppv7j4w5k2gkz4894y8nz";
      fetchSubmodules = true;
    };
  };

in {
  options.programs.flummbot = {
    enable = mkEnableOption "Small IRC bot in go used for my channels";

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
