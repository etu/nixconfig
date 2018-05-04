{ config, lib, pkgs, ... }:

with lib;

{
  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

  # Use local nixpkgs checkout
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nixpkgs"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  # Local overlays
  nixpkgs.overlays = [
    (import ../overlays/local/pkgs/default.nix)
  ];

  imports = [
    ../overlays/local/modules/default.nix
  ];

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  # Enable fish.
  programs.fish.enable = true;

  # Enable mosh.
  programs.mosh.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    curl
    file
    fzf
    git
    host
    htop
    ncdu
    pv
    ripgrep
    tmux
    whois
  ];

  # Enable firewall.
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

  # SSH Keys for remote logins
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC30zMfq5/ZBnLPXiz4qtTsg3SU6voKQMumADNhTpVSKo3erANR5zTb1WPfjM4IWLCcWfksWDNOOeMaKM0hGgdnGbfrXpIOJwKNHaSp11cvQ6wTMAGV3B3ItJHOV+Czw4kEeUB+Tic8m+U2jnTPLXC4x3B7bdXHhdhmQbTpEq9pabe8eRQM54/9SuG6M9y8G3g35s3edsXrEnh/OI62a66F5aOugQH4fX5ehfGg3zk7LLu7U8bX9FGOeOVCCEBsfm5ysczNAO3v1iA4G9N8vgfAHJNZfKglYSQIi9nyURxcqT511OFTGK1cyWHGjqCNK286Plx90u0SVQvvG+9hkq2l/kbgmNpEYlTmAs22y+6j7R+gpSCMxJSfjXfJeyVcCTKo2CT9+SQRDz+pz+wyv/NgnaqXuP65RlwS0OIhdT6YheaFfbhkuMzFD78VDWOacLamWVQz/yTe5o+GhTavWVZyZ4Y9Wf+LB4sQtM9S5AqWpSCHDfA9nF6E9oWPbAZ4l+VD4vNQdrmByh+3uk+XP9/ytJVyJDd88MmxnC1yiN3xT7rQaUoUCmYy0Z7BLBYvBb2fU+JrJ8Eew6uf23SGYUtZuxanNANslBdvD0t68xCEHIxsVecpqRtbG8699bZEoWWbAFS1WO5EBAcYRfKV/4SQxLTO0J+HGQSRkgL7Ex1ioQ== etu@phouchg-4096-2016-04-18"
  ];

  # Root shell
  users.extraUsers.root.shell = pkgs.fish;
}
