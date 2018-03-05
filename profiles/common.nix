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

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "dvorak-sv-a1";
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "all"
    ];
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "no";
  services.openssh.passwordAuthentication = false;

  # Enable fish
  programs.fish.enable = true;
  programs.mosh.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    ag
    bc                # Dependency for some fish functions
    ccid              # Used for smartcards
    curl
    dnsutils
    emacs25-nox
    file
    fzf
    git
    gnupg
    host
    htop
    jq
    ncdu
    nfs-utils
    oathToolkit       # Used by the pass otp etxension
    pass
    pv
    ripgrep
    sshfs-fuse
    stow
    testssl
    tmux
    whois
    youtube-dl

    nixUnstable

    # PHP utils
    php
    phpPackages.composer
    phpPackages.xdebug
    phpPackages.phpcbf
    phpPackages.phpcs

    # Golang tools
    go
    go-dependency-manager
    gocode
  ];

  # Enable firewall.
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

  # Define a user account.
  users.extraUsers.etu.isNormalUser = true;
  users.extraUsers.etu.uid = 1000;
  users.extraUsers.etu.description = "Elis Hirwing,,,,";
  users.extraUsers.etu.extraGroups = [ "wheel" ];
  users.extraUsers.etu.shell = pkgs.fish;

  # SSH Keys for remote logins
  users.extraUsers.etu.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPsVycIM8SAlMRIoytIyCqdHJ+ORAiPRAMR/lo5USVeg etu@prosser-2017-07-09"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC30zMfq5/ZBnLPXiz4qtTsg3SU6voKQMumADNhTpVSKo3erANR5zTb1WPfjM4IWLCcWfksWDNOOeMaKM0hGgdnGbfrXpIOJwKNHaSp11cvQ6wTMAGV3B3ItJHOV+Czw4kEeUB+Tic8m+U2jnTPLXC4x3B7bdXHhdhmQbTpEq9pabe8eRQM54/9SuG6M9y8G3g35s3edsXrEnh/OI62a66F5aOugQH4fX5ehfGg3zk7LLu7U8bX9FGOeOVCCEBsfm5ysczNAO3v1iA4G9N8vgfAHJNZfKglYSQIi9nyURxcqT511OFTGK1cyWHGjqCNK286Plx90u0SVQvvG+9hkq2l/kbgmNpEYlTmAs22y+6j7R+gpSCMxJSfjXfJeyVcCTKo2CT9+SQRDz+pz+wyv/NgnaqXuP65RlwS0OIhdT6YheaFfbhkuMzFD78VDWOacLamWVQz/yTe5o+GhTavWVZyZ4Y9Wf+LB4sQtM9S5AqWpSCHDfA9nF6E9oWPbAZ4l+VD4vNQdrmByh+3uk+XP9/ytJVyJDd88MmxnC1yiN3xT7rQaUoUCmYy0Z7BLBYvBb2fU+JrJ8Eew6uf23SGYUtZuxanNANslBdvD0t68xCEHIxsVecpqRtbG8699bZEoWWbAFS1WO5EBAcYRfKV/4SQxLTO0J+HGQSRkgL7Ex1ioQ== etu@phouchg-4096-2016-04-18"
    "no-agent-forwarding,no-X11-forwarding,permitopen=\"localhost:8001\",command=\"echo 'This account can only be used for weechat relays'\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDELIFRmA2C93sjHebScoj8QPynqdYyl6fgYLOrBMsBKQAKrzsfF4wmA/LYo9Z89l3TmpMqzd4C/315HFO6sO7iHVrUfsC0lToA+FOcN7D40pr8m+AaQtVSI14Mlz4GY3fyeyYyssz7XXMn9LEzgZ8SxZh06YLJM9yL1kprBoRXe3Bxbja38JBSl+8xBWRyNrQBPySrTeuoxRYbJ8DUwtOeSElSP6YDjtMut4PbjLXJ2GNHavXhoQaLiZsW4c4YzcMzjiKEmAZWNg2cNuljXMf3KoKCbxqiD9zWidWhKdMuT+XhuDzTt89JAdWWStkj2N++eeESRozHmDBp9PROJx7Z etu@android-2017-04-24"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDjJcOzWajRgpgZ/uHRNUSpm6giWoXAQL995e5mE0J3aZc/aF68svXjhvpjkRJRFMWJUkwngtQiP+qdFAFDdKI2t9sMeMRuI54HRIQUoeDou8dNPcTlXQjV2DcdOt7GedjlAexdzV/WitNlZC255a8200iQ7e5D9WjSibHpeBrMLhxYGGd/Lzvs5pn6jTJaTFVlCiosh/vnP/rP+W1ruWymVdMxe/6s3ExeIxPeWmpR8zuk30izJ+Gb2jCAzH5lWjpL8BYinVkhbxIyqouaa0Ycgs1/ohcKvRC5Sg/2Or2pJ8rol1aTq1Zb7Z0j+DaXtF3smnpm/gWnuFwin6xiuMjyR20ItI/VOBjZZcLxtDvleXCpBTcZHBaCoGl2YvuZ2GpDSTtF8TPCb3LDQry9LXFUULH650u8bCQvktKGUnuVnwte7wVQGLgTUR+0PsyE+yK34BPWAGxKtowxfMQ59szKo4bY/+WZyll3fySsBeAsGvNPHH9D+Uu3B28Aufyn/1nVQLJKH3UjcVhOeJpnVGw8VBEh8l6gt9wa2VQFP9Wpnlxpbpfr9Q5p6Ssmj4l+fal+PwElN36ltmDs9tfAvoNgxBiG9f0zwYq/pO5PEvmZCTrDqk/mPqEnZrhSp47Qx65ikwwD88ySs5HNtOgZ3NhrYRk4IacWRMt5ESWvJgDMXQ== openpgp:0x14E6F255"
  ];

  # Root shell
  users.extraUsers.root.shell = pkgs.fish;
}
