# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:
let
  # Import my ssh public keys
  keys = (import ../../data.nix).pubkeys;
in
{
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Include static network settings.
    ./networking.nix

    # Import local modules
    ../../modules
  ];

  # Set hostname
  networking.hostName = "vps05";

  # Settings needed for ZFS
  networking.hostId = "8425e349";

  # My module settings
  etu = {
    stateVersion = "22.11";

    base.emacs.enable = lib.mkForce false;
    user.extraRootAuthorizedKeys = keys.etu.syncoid.fenchurch;
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = [ "data" ];
    };
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Install mosh
  programs.mosh.enable = true;

  # Set up NGiNX
  services.nginx.enable = true;
}
