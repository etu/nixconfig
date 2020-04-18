{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.user;
  uid = cfg.uid;
  username = cfg.username;
  extraGroups = cfg.extraGroups;
  extraAuthorizedKeys = cfg.extraAuthorizedKeys;

  # Import my ssh public keys
  keys = import ../data/pubkeys.nix;

in {
  options.my.user = {
    enable = mkEnableOption "Enables my user.";
    uid = mkOption {
      type = types.nullOr types.int;
      default = 1000;
    };
    username = mkOption {
      type = types.str;
      default = "etu";
      description = "My username for this system.";
    };
    extraGroups = mkOption {
      type = types.listOf types.str;
      default = [];
    };
    extraAuthorizedKeys = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional authorized keys";
    };
    persistent = {
      homeDir = mkOption {
        type = types.str;
        default = "/persistent/home/etu";
        description = "Location of persistent home files";
      };
      extraFiles = mkOption {
        type = types.listOf types.str;
        default = [];
      };
      extraDirectories = mkOption {
        type = types.listOf types.str;
        default = [];
      };
    };
  };

  config = mkIf cfg.enable {
    # Let ~/bin/ be in $PATH
    environment.homeBinInPath = true;

    # Define my user account
    users.extraUsers.${username} = {
      isNormalUser = true;
      uid = uid;
      description = "Elis Hirwing,,,,";
      extraGroups = [ "wheel" ] ++ extraGroups;
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = with keys.etu;
        fenchurch ++ agrajag ++ ford-x250 ++ work ++ extraAuthorizedKeys;
    };

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      ag
      direnv
      dnsutils
      jq
      nfs-utils
      sshfs-fuse
      stow
      testssl
      youtube-dl

      # PHP utils
      php
      php.packages.composer
      php.packages.phpcbf
      php.packages.phpcs
    ];
  };
}
