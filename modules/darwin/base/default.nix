{
  lib,
  ...
}:
{
  imports = [
    ./emacs
    ./fish
    ./htop
    ./nix
    ./packages
    ./spell
    ./ssh
    ./system
    ./tailscale
    ./tmux
  ];

  options.etu = {
    dataPrefix = lib.mkOption {
      type = lib.types.str;
      # macOS has no ephemeral root, so everything persists in place.
      default = "";
      description = "The path to where persistent storage happens";
    };
  };

  config = {
    # Enable base services.
    etu.base = {
      emacs.enable = lib.mkDefault true;
      fish.enable = lib.mkDefault true;
      htop.enable = lib.mkDefault true;
      nix.enable = lib.mkDefault true;
      spell.enable = lib.mkDefault true;
      ssh.enable = lib.mkDefault true;
      tailscale.enable = lib.mkDefault true;
      tmux.enable = lib.mkDefault true;
    };
  };
}
