{ config, lib, pkgs, ... }:

let
  # Import my ssh public keys
  keys = (import ../../data.nix).pubkeys;

in
{
  options.etu.user = {
    enable = lib.mkEnableOption "Enables my user";
    uid = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = 1000;
      description = "My user id for this system.";
    };
    username = lib.mkOption {
      type = lib.types.str;
      default = "etu";
      description = "My username for this system.";
    };
    realname = lib.mkOption {
      type = lib.types.str;
      default = "Elis Hirwing";
      description = "My realname for this system.";
    };
    email = lib.mkOption {
      type = lib.types.str;
      default = "elis@hirwing.se";
      description = "My email for this system.";
    };
    workEmail = lib.mkOption {
      type = lib.types.str;
      default = "elis.hirwing@schibsted.com";
      description = "My email for this system.";
    };
    signingKey = lib.mkOption {
      type = lib.types.str;
      default = "67FE98F28C44CF221828E12FD57EFA625C9A925F";
      description = "My public signing key for this system.";
    };
    extraGroups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
    extraAuthorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Additional authorized keys.";
    };
    defaultShell = lib.mkOption {
      type = lib.types.package;
      default = pkgs.fish;
      description = "Default shell to use.";
    };
  };

  config = lib.mkIf config.etu.user.enable {
    # Let ~/bin/ be in $PATH
    environment.homeBinInPath = true;

    # Define my user account
    users.users.${config.etu.user.username} = {
      isNormalUser = true;
      uid = config.etu.user.uid;
      description = "${config.etu.user.realname},,,,";
      extraGroups = [ "wheel" ] ++ config.etu.user.extraGroups;
      shell = config.etu.user.defaultShell;
      openssh.authorizedKeys.keys = keys.etu.computers ++ config.etu.user.extraAuthorizedKeys;
    };
  };
}
