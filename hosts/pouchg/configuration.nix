# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../profiles/common.nix
    ../../profiles/graphical-desktop.nix
    ../../profiles/vbox.nix
  ];

  networking.hostName = "pouchg";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # Disable CUPS to print documents.
  services.printing.enable = true;

  # Open all ports from 192.168.5.102.
  networking.firewall.extraCommands = ''
  # Allow any traffic from virtual machine to host (mostly NFS).
  iptables -A INPUT -d 192.168.5.1 -s 192.168.5.102 -j ACCEPT
  '';

  # Enable nfs server.
  services.nfs.server.enable = true;
  services.nfs.server.exports = ''
    /home/etu/tvnu/projects 192.168.5.102(rw,no_subtree_check,all_squash,anonuid=1000,anongid=100)
  '';
}
