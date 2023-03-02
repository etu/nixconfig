# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix
  ];

  # Set hostname
  networking.hostName = "zparv-zerver";

  # Settings needed for ZFS
  networking.hostId = "48a0ce30";

  # Enable weekly garbage-collection and daily store optimization.
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
  nix.optimise.automatic = true;
  nix.optimise.dates = ["daily"];

  # My module settings
  etu = {
    stateVersion = "23.05";

    # Set data prefix so agenix can find the host keys.
    dataPrefix = "/";

    # We do have a persistent root on this system, I know, it's
    # weird. So here I disable the persstence settings for this system
    # and just keep the files on /
    base.zfs.enable = lib.mkForce false;

    base.emacs.enable = lib.mkForce false;
    base.telegraf.enable = true;
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/root".use_template = ["data"];
      "zroot/safe/home".use_template = ["data"];
    };
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;
}
