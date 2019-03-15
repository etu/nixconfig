{ config, lib, pkgs, ... }:

with lib;

let
 cfg = config.my.user;
 uid = cfg.uid;
 username = cfg.username;
 extraGroups = cfg.extraGroups;
 extraAuthorizedKeys = cfg.extraAuthorizedKeys;

in {
  options.my.user = {
    enable = mkEnableOption "Enables my user.";
    uid = mkOption {
      type = types.nullOr types.int;
      default = 1000;
    };
    username = mkOption {
      type = types.string;
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
  };

  config = mkIf cfg.enable {
    # Define my user account
    users.extraUsers.${username} = {
      isNormalUser = true;
      uid = uid;
      description = "Elis Hirwing,,,,";
      extraGroups = [ "wheel" ] ++ extraGroups;
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC30zMfq5/ZBnLPXiz4qtTsg3SU6voKQMumADNhTpVSKo3erANR5zTb1WPfjM4IWLCcWfksWDNOOeMaKM0hGgdnGbfrXpIOJwKNHaSp11cvQ6wTMAGV3B3ItJHOV+Czw4kEeUB+Tic8m+U2jnTPLXC4x3B7bdXHhdhmQbTpEq9pabe8eRQM54/9SuG6M9y8G3g35s3edsXrEnh/OI62a66F5aOugQH4fX5ehfGg3zk7LLu7U8bX9FGOeOVCCEBsfm5ysczNAO3v1iA4G9N8vgfAHJNZfKglYSQIi9nyURxcqT511OFTGK1cyWHGjqCNK286Plx90u0SVQvvG+9hkq2l/kbgmNpEYlTmAs22y+6j7R+gpSCMxJSfjXfJeyVcCTKo2CT9+SQRDz+pz+wyv/NgnaqXuP65RlwS0OIhdT6YheaFfbhkuMzFD78VDWOacLamWVQz/yTe5o+GhTavWVZyZ4Y9Wf+LB4sQtM9S5AqWpSCHDfA9nF6E9oWPbAZ4l+VD4vNQdrmByh+3uk+XP9/ytJVyJDd88MmxnC1yiN3xT7rQaUoUCmYy0Z7BLBYvBb2fU+JrJ8Eew6uf23SGYUtZuxanNANslBdvD0t68xCEHIxsVecpqRtbG8699bZEoWWbAFS1WO5EBAcYRfKV/4SQxLTO0J+HGQSRkgL7Ex1ioQ== etu@phouchg-4096-2016-04-18"
      ] ++ extraAuthorizedKeys;
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
      phpPackages.composer
      phpPackages.xdebug
      phpPackages.phpcbf
      phpPackages.phpcs
    ];
  };
}
