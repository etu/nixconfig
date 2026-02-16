{
  config,
  pkgs,
  modulesPath,
  lib,
  inputs,
  ...
}:
{
  imports = [
    # Hardware settings
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-gpu-intel

    # Scanned modules
    (modulesPath + "/installer/scan/not-detected.nix")

    # Filesystem layout
    ./disko.nix
  ];

  # Configure boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set boot loader timeout to longer than 5s to give me more time to choose.
  boot.loader.timeout = 15;

  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "thunderbolt"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];

  boot.kernelModules = [ "kvm-amd" ];

  # Enable a nice boot splash screen.
  boot.initrd.systemd.enable = true; # needed for ZFS password prompt with plymouth.
  boot.plymouth.enable = true;

  # Enable ZFS.
  boot.supportedFilesystems = [ "zfs" ];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Install firmware for hardware.
  hardware.enableRedistributableFirmware = true;

  # Test to use OpenRGB to see if it works with my RGB
  services.hardware.openrgb.enable = true;
  services.hardware.openrgb.motherboard = "amd";

  # Enable openrazer to control razer devices.
  hardware.openrazer.enable = true;
  hardware.openrazer.users = [ config.etu.user.username ];
  etu.user.extraUserPackages = [ pkgs.polychromatic ];

  # Use a newer version of OpenRGB to get support for my motherboard.
  #
  # There's a PR for future updates to upstream: https://github.com/NixOS/nixpkgs/pull/381881
  #services.hardware.openrgb.package = pkgs.openrgb.overrideAttrs (oa: {
  #  version = "1.0rc2";
  #
  #  src = pkgs.fetchFromGitLab {
  #    inherit (oa.src) owner repo;
  #    rev = "release_candidate_1.0rc2";
  #    sha256 = "sha256-vdIA9i1ewcrfX5U7FkcRR+ISdH5uRi9fz9YU5IkPKJQ=";
  #  };
  #
  #  postPatch = ''
  #    substituteInPlace scripts/build-udev-rules.sh \
  #      --replace-fail "/usr/bin/env" "${pkgs.lib.getExe' pkgs.coreutils "env"}" \
  #      --replace-fail chmod "${pkgs.lib.getExe' pkgs.coreutils "chmod"}"
  #
  #    substituteInPlace OpenRGB.pro \
  #      --replace-fail "/etc/systemd/system" "$out/etc/systemd/system"
  #  '';
  #});

  # Enable fwupd for firmware updates etc.
  services.fwupd.enable = true;

  # Mark filesystems as needed for boot
  fileSystems.${config.etu.dataPrefix}.neededForBoot = true;
  fileSystems."${config.etu.dataPrefix}/home".neededForBoot = true;
  fileSystems.${config.etu.localPrefix}.neededForBoot = true;
  fileSystems."/nix".neededForBoot = true;

  # Bind mount for persistent libvirt state.
  etu.base.zfs.system.directories = [
    "/var/lib/libvirt"
  ];

  # Swap devices.
  swapDevices = [ ];

  # Set max jobs in nix.
  #nix.settings.max-jobs = lib.mkDefault 8;

  # Use the host platform for building by default to avoid cross compiling.
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # Set CPU Frequency Governor.
  #powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
