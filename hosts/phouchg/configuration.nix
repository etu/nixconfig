# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  # Declare download path for home-manager to avoid the need to have it as a channel
  home-manager = builtins.fetchTarball {
    url = "https://github.com/rycee/home-manager/archive/master.tar.gz";
  };
in {
  imports = [
    ./hardware-configuration.nix

    # Import local modules
    ../../modules

    # Import the home-manager module
    "${home-manager}/nixos"
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.09";

  networking.hostName = "phouchg";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Install thinkpad modules for TLP
  boot.extraModulePackages = [
    pkgs.linuxPackages.acpi_call
  ];

  boot.cleanTmpDir = true;

  # Hardware settings
  services.xserver.videoDrivers = ["intel" "modesetting"];
  hardware.cpu.intel.updateMicrocode = true;

  # Enable fwupd for firmware updates etc
  services.fwupd.enable = true;

  # Enable TLP
  services.tlp.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable nfs server exports.
  services.nfs.server.exports = ''
    /home/etu/tvnu/projects 192.168.5.102(rw,no_subtree_check,all_squash,anonuid=1000,anongid=100)
  '';

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable gpg related stuff
  my.gpg-utils.enable = true;

  # Enable common graphical stuff
  my.common-graphical.enable = true;

  # Enable emacs deamon stuff
  my.emacs.enable = true;

  # Enable my exwm desktop settings
  my.desktop-exwm.enable = true;

  # Define a user account.
  my.user.enable = true;
  my.user.extraGroups = [
    "docker"
  ];

  # Enable nfsd with firewall rules.
  my.nfsd.enable = true;

  # Enable vbox and friends.
  my.vbox.enable = true;

  # Home-manager as nix module
  home-manager.users.etu = import ../../home-etu-nixpkgs/home.nix;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    sequeler
    docker-compose
  ];

  virtualisation.docker.enable = true;
}
