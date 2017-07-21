{ config, lib, pkgs, ... }:

with lib;

{
  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

  # Use local nixpkgs checkout
  nix.nixPath = [ "/etc/nixos" "nixos-config=/etc/nixos/configuration.nix" ];

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "dvorak-sv-a1";
    defaultLocale = "en_US.UTF-8";
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "no";
  services.openssh.passwordAuthentication = false;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    fish
    curl
    emacs
    htop
    git
    gnupg
    ccid
    nfs-utils
    file
    dnsutils
    host
    whois
  ];

  # Define a user account.
  users.extraUsers.etu.isNormalUser = true;
  users.extraUsers.etu.uid = 1000;
  users.extraUsers.etu.extraGroups = [ "wheel" ];
  users.extraUsers.etu.shell = pkgs.fish;

  # Root shell
  users.extraUsers.root.shell = pkgs.fish;
}
