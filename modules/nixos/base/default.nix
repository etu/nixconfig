{
  lib,
  ...
}:
{
  imports = [
    ./emacs
    ./fish
    ./htop
    ./locale
    ./nix
    ./packages
    ./sanoid
    ./spell
    ./sshd
    ./syncoid
    ./system
    ./tailscale
    ./tmux
    ./zfs
  ];

  options.etu = {
    stateVersion = lib.mkOption {
      type = lib.types.str;
      example = "22.05";
      description = "The NixOS state version to use for this system";
    };
    dataPrefix = lib.mkOption {
      type = lib.types.str;
      default = "/data";
      description = "The path to where persistent storage happens";
    };
    localPrefix = lib.mkOption {
      type = lib.types.str;
      default = "/data/local";
      description = "The path to where persistent local storage happens";
    };
  };

  config = {
    # Blacklist compromised CA certificates. The CA bundle build fails on
    # entries that aren't in the store any more, so drop an entry once nss
    # removes the certificate itself (as 3.131.1 did with "emSign ECC Root
    # CA - G3").
    security.pki.caCertificateBlacklist = [
      "Comodo AAA Services root"
    ];

    # Enable base services.
    etu.base = {
      emacs.enable = lib.mkDefault true;
      fish.enable = lib.mkDefault true;
      htop.enable = lib.mkDefault true;
      tmux.enable = lib.mkDefault true;
      nix.enable = lib.mkDefault true;
      sshd.enable = lib.mkDefault true;
      sanoid.enable = lib.mkDefault true;
      spell.enable = lib.mkDefault true;
      tailscale.enable = lib.mkDefault true;
      zfs.enable = lib.mkDefault true;
    };
  };
}
