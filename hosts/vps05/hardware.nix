{ lib, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
  ];

  # Configure boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];

  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # Enable ZFS.
  boot.supportedFilesystems = [ "zfs" ];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Filesystem mounts.
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=1G" "mode=755" ];
  };

  fileSystems."/nix" = {
    device = "zroot/nix";
    fsType = "zfs";
  };

  fileSystems."/var/log" = {
    device = "zroot/var-log";
    fsType = "zfs";
  };

  fileSystems."/persistent" = {
    device = "zroot/persistent";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C5E1-A34A";
    fsType = "vfat";
  };

  # Persistence of certain hosts paths and home directory paths.
  environment.persistence."/persistent".files = [
    "/etc/machine-id"
    "/etc/ssh/ssh_host_rsa_key"
    "/etc/ssh/ssh_host_rsa_key.pub"
    "/etc/ssh/ssh_host_ed25519_key"
    "/etc/ssh/ssh_host_ed25519_key.pub"
  ];

  # Persistence of gitea and postgresql data files between boots
  fileSystems."/var/lib/gitea" = {
    device = "/persistent/var/lib/gitea";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  fileSystems."/var/lib/postgresql" = {
    device = "/persistent/var/lib/postgresql";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Bind mount for persistent certificates for nginx
  fileSystems."/var/lib/acme" = {
    device = "/persistent/var/lib/acme";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Persistence of roots dotfiles between boots
  fileSystems."/root" = {
    device = "/persistent/home/root";
    options = [ "bind" "noauto" "x-systemd.automount" ];
    noCheck = true;
  };

  # Swap devices.
  swapDevices = [ ];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 1;
}