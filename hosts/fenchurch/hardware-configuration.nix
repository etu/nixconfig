# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=10G" "mode=755" ];
  };

  fileSystems."/nix" = {
    device = "zroot/nix";
    fsType = "zfs";
  };

  fileSystems."/persistent" = {
    device = "zroot/persistent";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/persistent/home" = {
    device = "zroot/home";
    fsType = "zfs";
  };

  fileSystems."/var/log" = {
    device = "zroot/var-log";
    fsType = "zfs";
  };

  fileSystems."/var/lib/nzbget-dst" = {
    device = "zroot/var-lib-nzbget-dst";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/6241-1BC1";
    fsType = "vfat";
    options = [ "noauto" "x-systemd.automount" ];
  };

  fileSystems."/boot-fallback" = {
    device = "/dev/disk/by-uuid/6258-01A0";
    fsType = "vfat";
    options = [ "noauto" "x-systemd.automount" ];
  };

  # Define mount point for the raid
  fileSystems."/media/legacy" = {
    device = "/dev/mapper/cryptraid";
    fsType = "ext4";
    options = [ "noauto" "x-systemd.automount" ];
    encrypted = {
      blkDev = "/dev/disk/by-uuid/c8454f1f-39eb-49f9-9756-a69c41068ede";
      label = "cryptraid";
    };
  };

  # Write a crypttab file for the raid
  environment.etc.crypttab.text = "cryptraid UUID=c8454f1f-39eb-49f9-9756-a69c41068ede /persistent/etc/cryptraid_keyfile1";

  # And install cryptsetup to unlock the raid
  environment.systemPackages = with pkgs; [ cryptsetup ];

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
