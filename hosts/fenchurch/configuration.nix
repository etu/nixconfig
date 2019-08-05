# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./persistence.nix

    # Import local modules
    ../../modules

    # Import the home-manager module
    <home-manager/nixos>
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.09";

  networking.hostName = "fenchurch";

  # Hardware settings
  hardware.cpu.intel.updateMicrocode = true;

  # Boot loader settings
  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";

  boot.loader.grub.mirroredBoots = [
    { devices = [ "/dev/disk/by-uuid/6258-01A0" ]; path = "/boot-fallback"; }
  ];

  # Settings needed for ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "23916528";
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true;

  # Remote unlocking of encrypted ZFS
  boot.initrd = {
    kernelModules = [ "e1000e" ];
    network.enable = true;
    # Listen to ssh to let me decrypt zfs
    network.ssh = {
      enable = true;
      port = 2222;
      hostECDSAKey = /persistent/initrd-ssh-key;
      authorizedKeys = config.users.users.etu.openssh.authorizedKeys.keys;
    };
    # Prompt me for password to decrypt zfs
    network.postCommands = ''
        echo "zfs load-key -a; killall zfs" >> /root/.profile
    '';
  };
  networking.useDHCP = true;

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable emacs deamon stuff
  my.emacs.enable = true;

  # Define a user account.
  my.user.enable = true;
  my.user.extraGroups = [
    "libvirtd"
  ];

  users.users.root.initialHashedPassword = "$6$f0a4BXeQkQ719H$5zOS.B3/gDqDN9/1Zs20JUCCPWpzkYmOx6XjPqyCe5kZD5z744iU8cwxRyNZjPRa63S2oTml7QizxfS4jjMkE1";
  users.users.etu.initialHashedPassword = "$6$f0a4BXeQkQ719H$5zOS.B3/gDqDN9/1Zs20JUCCPWpzkYmOx6XjPqyCe5kZD5z744iU8cwxRyNZjPRa63S2oTml7QizxfS4jjMkE1";

  # Home-manager as nix module
  home-manager.users.etu = import ../../home-etu-nixpkgs/home.nix;

  # Enable kvm
  virtualisation.libvirtd.enable = true;
}
