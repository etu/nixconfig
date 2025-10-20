{
  config,
  pkgs,
  modulesPath,
  lib,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
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

  # Wifi needs at least 6.11, system works overall well with 6.12
  # (with a separate network card) and built in ethernet works
  # starting with 6.13. Then bluetooth audio starts to work in 6.14,
  # interestingly, Wifi stops working on 6.14, Wifi then starts
  # working on 6.14.11. Now 6.14 is EOL so 6.15 has to be tested.
  # Wifi also stops working on 6.15.3. And keeps not working on
  # 6.15.4. Then WiFi starts working on 6.15.9, and bluetooth is still
  # fine. Next step, 6.16.4, and bluetooth is still fine, and WiFi is
  # also fine.
  #
  # Network Card:
  # $ lspci -s 10:00.0 -nn -k
  # 10:00.0 Network controller [0280]: Qualcomm Technologies, Inc WCN785x Wi-Fi 7(802.11be) 320MHz 2x2 [FastConnect 7800] [17cb:1107] (rev 01)
  #         Subsystem: Foxconn International, Inc. Device [105b:e0fb]
  #         Kernel driver in use: ath12k_pci
  #         Kernel modules: ath12k
  #
  # Logs on 6.13.12:
  # May 17 14:32:49 desktop-elis kernel: ath12k_pci 0000:10:00.0: BAR 0 [mem 0xa0000000-0xa01fffff 64bit]: assigned
  # May 17 14:32:50 desktop-elis kernel: ath12k_pci 0000:10:00.0: enabling device (0000 -> 0002)
  # May 17 14:32:50 desktop-elis kernel: ath12k_pci 0000:10:00.0: MSI vectors: 16
  # May 17 14:32:50 desktop-elis kernel: ath12k_pci 0000:10:00.0: Hardware name: wcn7850 hw2.0
  # May 17 14:33:00 desktop-elis kernel: ath12k_pci 0000:10:00.0: chip_id 0x2 chip_family 0x4 board_id 0xff soc_id 0x40170200
  # May 17 14:33:00 desktop-elis kernel: ath12k_pci 0000:10:00.0: fw_version 0x1105811c fw_build_timestamp 2025-03-11 07:08 fw_build_id QC_IMAGE_VERSION_STRING=WLAN.HMT.1.1.c5-00284-QCAHMTSWPL_V1.0_V2.0_SILICONZ-3
  #
  # Logs on 6.14.6:
  # May 17 14:36:00 desktop-elis kernel: ath12k_pci 0000:10:00.0: BAR 0 [mem 0xa0000000-0xa01fffff 64bit]: assigned
  # May 17 14:36:00 desktop-elis kernel: ath12k_pci 0000:10:00.0: enabling device (0000 -> 0002)
  # May 17 14:36:00 desktop-elis kernel: ath12k_pci 0000:10:00.0: MSI vectors: 16
  # May 17 14:36:00 desktop-elis kernel: ath12k_pci 0000:10:00.0: Hardware name: wcn7850 hw2.0
  # May 17 14:36:01 desktop-elis kernel: ath12k_pci 0000:10:00.0: chip_id 0x2 chip_family 0x4 board_id 0xff soc_id 0x40170200
  # May 17 14:36:01 desktop-elis kernel: ath12k_pci 0000:10:00.0: fw_version 0x1105811c fw_build_timestamp 2025-03-11 07:08 fw_build_id QC_IMAGE_VERSION_STRING=WLAN.HMT.1.1.c5-00284-QCAHMTSWPL_V1.0_V2.0_SILICONZ-3
  # May 17 14:36:01 desktop-elis kernel: ath12k_pci 0000:10:00.0: ignore reset dev flags 0x200
  # May 17 14:36:06 desktop-elis kernel: ath12k_pci 0000:10:00.0: failed to receive wmi unified ready event: -110
  # May 17 14:36:06 desktop-elis kernel: ath12k_pci 0000:10:00.0: failed to start core: -110
  # May 17 14:36:06 desktop-elis kernel: ath12k_pci 0000:10:00.0: qmi failed to send mode request, mode: 4, err = -5
  # May 17 14:36:06 desktop-elis kernel: ath12k_pci 0000:10:00.0: qmi failed to send wlan mode off
  boot.kernelPackages = pkgs.linuxPackages_6_16;

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

  # Set video driver.
  services.xserver.videoDrivers = [ "modesetting" ];

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
