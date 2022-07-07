{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.etu.user.enable {
    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
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
    ];
  };
}
