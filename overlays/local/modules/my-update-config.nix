{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.update-config;
  git = cfg.package;

in {
  options = {
    my.update-config = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable auto updating of the nix config
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.git;
        defaultText = "pkgs.git";
        description = "git derivation to use";
      };
      user = mkOption {
        type = types.str;
        default = "root";
        defaultText = "root";
        description = "User to run as";
      };
      path = mkOption {
        type = types.str;
        default = "/etc/nixos/";
        defaultText = "/etc/nixos/";
        description = "Working directory for the job to update the config";
      };
      interval = mkOption {
        type = types.str;
        default = "04:30";
        example = "hourly";
        description = ''
          Run a gitea dump at this interval. Runs by default at 04:31 every day.

          The format is described in
          <citerefentry><refentrytitle>systemd.time</refentrytitle>
          <manvolnum>7</manvolnum></citerefentry>.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.update-nixos-config = {
      description = "update-nixos-config";
      after = [ "network.target" ];
      wantedBy = [ "default.target" ];
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        ExecStart = "${cfg.package}/bin/git pull";
        WorkingDirectory = cfg.path;
      };
    };
    systemd.timers.update-nixos-config = {
      description = "Update timer for update-nixos-config";
      partOf = [ "update-nixos-config.service" ];
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = cfg.interval;
    };
  };
}
