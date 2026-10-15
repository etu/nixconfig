# MacBook Air M5 managed with nix-darwin.
#
# Initial bootstrap (Nix must already be installed):
# 1. Move away the files nix-darwin wants to manage:
#    $ sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
# 2. Activate the configuration:
#    $ sudo nix --extra-experimental-features 'nix-command flakes' run nix-darwin -- switch --flake .#macbook-air-elis
#
# Later deployments:
# $ just switch-macbook-air-elis
{ pkgs, ... }:
{
  imports = [
    ./etu-options.nix
  ];

  nixpkgs.hostPlatform = "aarch64-darwin";

  # Set the hostname so darwin-rebuild picks this configuration by default.
  networking.hostName = "macbook-air-elis";
  networking.computerName = "macbook-air-elis";
  networking.localHostName = "macbook-air-elis";

  # User that user-specific options (defaults, homebrew, etc) apply to.
  system.primaryUser = "elishirwing";
  users.users.elishirwing.home = "/Users/elishirwing";

  # Nix settings, mirroring modules/nixos/base on the NixOS hosts.
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nix.settings.trusted-users = [
    "root"
    "elishirwing"
  ];
  nix.settings.substituters = [
    "https://nix-community.cachix.org"
    "https://etu.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "etu.cachix.org-1:CeyfbMJZHZ95TScp8+I8+EeyzbncqPSj1xfCK9vOAFE="
  ];

  # Remap Caps Lock to Control.
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Install fish system wide, load the Nix environment in it and add it to
  # /etc/shells. nix-darwin only changes login shells of users listed in
  # users.knownUsers, so switch to it once with:
  # $ chsh -s /run/current-system/sw/bin/fish
  programs.fish.enable = true;
  environment.shells = [ pkgs.fish ];

  # Use zoxide for cd in fish, like on laptop-private-elis.
  etu.base.fish.enableUserZoxideCd = true;

  # Use Touch ID for sudo.
  security.pam.services.sudo_local.touchIdAuth = true;

  environment.systemPackages = [
    pkgs.just
    pkgs.nix-output-monitor
  ];

  # Used for backwards compatibility, read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
