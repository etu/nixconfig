# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  # Load secrets
  secrets = import ../../data/load-secrets.nix;

  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

in {
  imports = [
    ./hardware-configuration.nix
    ./persistence.nix

    # Import local modules
    ../../modules
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.03";

  networking.hostName = "guest-x240";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.extraModulePackages = [
    pkgs.linuxPackages.acpi_call
  ];

  # Fix touchpad scrolling after suspend.
  boot.kernelParams = [ "psmouse.synaptics_intertouch=0" ];

  boot.cleanTmpDir = true;

  # Hardware settings
  services.xserver.videoDrivers = ["intel" "modesetting"];
  hardware.trackpoint.enable = true;
  hardware.cpu.intel.updateMicrocode = true;

  # Enable TLP
  services.tlp.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Enable emacs deamon stuff
  my.emacs.enable = true;

  # Enable dmrconfig to configure my hamradio.
  programs.dmrconfig.enable = true;

  networking.networkmanager.enable = true;

  # Networkmanager with network online target
  # This is a hack to make my NFS not fail to mount 5 times before I get an IP
  # by DHCP on boot: https://github.com/NixOS/nixpkgs/pull/60954
  systemd.services.NetworkManager-wait-online = {
    wantedBy = [ "network-online.target" ];
  };

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    firefox-bin
    mpv
    sgtpuzzles
    superTuxKart

    fsarchiver
    ntfsprogs
  ];

  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.sddm.autoLogin.enable = true;
  services.xserver.displayManager.sddm.autoLogin.relogin = true;
  services.xserver.displayManager.sddm.autoLogin.user = "guest";
  services.xserver.libinput.enable = true;

  # Keyboard layout.
  services.xserver.layout = "se";
  services.xserver.xkbOptions = "eurosign:e,ctrl:nocaps,numpad:mac,kpdl:dot";
  services.xserver.xkbVariant = "dvorak";

  # Create a guest user
  users.users.guest.isNormalUser = true;
  users.users.guest.initialPassword = "";
  users.users.guest.shell = pkgs.fish;

  # Immutable users due to tmpfs
  users.mutableUsers = false;

  # Set up root user
  users.users.root.initialHashedPassword = secrets.hashedRootPassword;
  users.users.root.openssh.authorizedKeys.keys = with keys.etu; fenchurch ++ agrajag ++ work;
}
