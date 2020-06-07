{ config, lib, pkgs, ... }:

let
  cfg = config.my.user;
  uid = cfg.uid;
  username = cfg.username;
  extraGroups = cfg.extraGroups;
  extraAuthorizedKeys = cfg.extraAuthorizedKeys;

  # Import my ssh public keys
  keys = import ../data/pubkeys.nix;

in
{
  options.my.user = {
    enable = lib.mkEnableOption "Enables my user.";
    uid = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = 1000;
    };
    username = lib.mkOption {
      type = lib.types.str;
      default = "etu";
      description = "My username for this system.";
    };
    extraGroups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
    extraAuthorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Additional authorized keys";
    };
    persistent = {
      homeDir = lib.mkOption {
        type = lib.types.str;
        default = "/persistent/home/etu";
        description = "Location of persistent home files";
      };
      extraFiles = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
      extraDirectories = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
    };
  };

  config = lib.mkIf cfg.enable {
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
        fenchurch ++ agrajag ++ work ++ extraAuthorizedKeys;
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
