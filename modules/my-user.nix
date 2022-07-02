{ config, lib, pkgs, ... }:
let
  cfg = config.my.user;
  uid = cfg.uid;
  username = cfg.username;
  extraGroups = cfg.extraGroups;
  extraAuthorizedKeys = cfg.extraAuthorizedKeys;

  # Import my ssh public keys
  keys = (import ../data.nix).pubkeys;

in
{
  config = lib.mkIf cfg.enable {
    # Let ~/bin/ be in $PATH
    environment.homeBinInPath = true;

    # Define my user account
    users.extraUsers.${username} = {
      isNormalUser = true;
      uid = uid;
      description = "${config.my.user.realname},,,,";
      extraGroups = [ "wheel" ] ++ extraGroups;
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = keys.etu.computers ++ extraAuthorizedKeys;
    };

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      direnv
      dnsutils

      # Parse different formats and command outputs to json
      jc

      # Parse json
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
