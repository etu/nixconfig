{ config, lib, pkgs, ... }:
let
  # Import my ssh public keys
  keys = import ../data/pubkeys.nix;

  cfg = config.my.deploy-user;

in
{
  options.my.deploy-user.enable = lib.mkEnableOption "Enables my deploy user";
  config = lib.mkIf cfg.enable {
    # Add a deploy user for deployments
    users.users.deploy = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = keys.etu.computers;
    };

    # It needs to be a trusted user to copy things to the store
    nix.trustedUsers = [ "deploy" ];

    # And it needs to be able to execute things as sudo at arbitrary
    # paths to be able to switch generations and things like that.
    security.sudo.extraRules = [{
      users = [ "deploy" ];
      commands = [{
        command = "ALL";
        options = [ "SETENV" "NOPASSWD" ];
      }];
    }];
  };
}
