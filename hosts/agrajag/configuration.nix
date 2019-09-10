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
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./persistence.nix

    # Import local modules
    ../../modules

    # Import the home-manager module
    "${home-manager}/nixos"
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.09";

  # Use local nixpkgs checkout
  nix.nixPath = [
    "nixos-config=/etc/nixos/configuration.nix"
    "nixpkgs=/etc/nixos/nixpkgs"
  ];

  # Set hostname
  networking.hostName = "agrajag";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_5_2;

  # Install thinkpad modules for TLP
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

  # Settings needed for ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "27416952";
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true;

  # Hardware settings
  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;

  # Include udev rules to give permissions to the video group to change
  # backlight using acpilight.
  hardware.acpilight.enable = true;

  # Set video driver
  services.xserver.videoDrivers = [ "modesetting" ];

  # Enable fwupd for firmware updates etc
  services.fwupd.enable = true;

  # Enable TLP
  services.tlp.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [
    pkgs.postscript-lexmark
  ];

  # Enable printer configuration
  hardware.printers.ensureDefaultPrinter = "Lexmark_CS510de";
  hardware.printers.ensurePrinters = [
    {
      name = "Lexmark_CS510de";
      deviceUri = "ipps://192.168.0.124:443/ipp/print";
      model = "postscript-lexmark/Lexmark-CS510_Series-Postscript-Lexmark.ppd";
      location = "UFS";
      ppdOptions = {
        PageSize = "A4";
      };
    }
  ];

  # Enable SANE to handle scanners
  hardware.sane.enable = true;

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

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
  my.user.extraGroups = [ "video" ];

  users.users.root.initialHashedPassword = "$6$f0a4BXeQkQ719H$5zOS.B3/gDqDN9/1Zs20JUCCPWpzkYmOx6XjPqyCe5kZD5z744iU8cwxRyNZjPRa63S2oTml7QizxfS4jjMkE1";
  users.users.etu.initialHashedPassword = "$6$f0a4BXeQkQ719H$5zOS.B3/gDqDN9/1Zs20JUCCPWpzkYmOx6XjPqyCe5kZD5z744iU8cwxRyNZjPRa63S2oTml7QizxfS4jjMkE1";

  # Home-manager as nix module
  home-manager.users.etu = import ../../home-etu-nixpkgs/home.nix;
}
