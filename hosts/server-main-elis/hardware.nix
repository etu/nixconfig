{
  config,
  lib,
  modulesPath,
  myData,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Configure boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.extraInstallCommands = ''
    mkdir -p /boot/EFI/Microsoft/Boot/ /boot-fallback/EFI/Microsoft/Boot/
    cp /boot/EFI/grub/grubx64.efi /boot/EFI/Microsoft/Boot/bootmgfw.efi
    cp /boot-fallback/EFI/grub/grubx64.efi /boot-fallback/EFI/Microsoft/Boot/bootmgfw.efi
  '';

  boot.loader.grub.mirroredBoots = [
    {
      devices = ["/dev/disk/by-uuid/6258-01A0"];
      path = "/boot-fallback";
    }
  ];

  boot.kernelModules = [];
  boot.extraModulePackages = [];

  age.secrets = {
    inherit (myData.ageModules) server-main-elis-initrd-sshd;
    inherit (myData.ageModules) syncoid-server-main-elis-ssh-ec;
  };

  # Remote unlocking of encrypted ZFS
  boot.initrd = {
    availableKernelModules = ["xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
    kernelModules = ["e1000e" "kvm-intel"];
    network.enable = true;
    # Listen to ssh to let me decrypt zfs
    network.ssh = {
      enable = true;
      port = 2222;
      hostKeys = [
        myData.ageModules.server-main-elis-initrd-sshd.path
      ];
      authorizedKeys = config.users.users.etu.openssh.authorizedKeys.keys;
    };
    # Prompt me for password to decrypt zfs
    #
    # This was fun, the reason it looks like this is because of the
    # initramfs that firsts imports a pool, then it stalls on running
    # "zfs load-key -a" on the terminal, but we never input data there
    # since it's on SSH. So I have to run that command when I log in,
    # then it has to kill the other zfs command to continue the init
    # script, that script will then import the second pool and the
    # same dance starts over.
    network.postCommands = ''
      echo "zfs load-key -a; killall zfs; sleep 5; zfs load-key -a; killall zfs;" >> /root/.profile
    '';
  };

  # Roll back certain filesystems to empty state on boot
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r zroot/local/var-lib-nzbget-dst@empty
  '';

  # Enable ZFS.
  boot.supportedFilesystems = ["zfs"];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Enable nvidia drivers.
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.open = false;

  # Allow certain unfree packages.
  etu.base.nix.allowUnfree = [
    "nvidia-x11"
    "nvidia-settings"
    "libXNVCtrl"
  ];

  # Filesystem mounts.
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = ["defaults" "size=10G" "mode=755"];
  };

  fileSystems."/nix" = {
    device = "zroot/local/nix";
    fsType = "zfs";
  };

  fileSystems.${config.etu.dataPrefix} = {
    device = "zroot/safe/data";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."${config.etu.dataPrefix}/home" = {
    device = "zroot/safe/home";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/var/log" = {
    device = "zroot/local/var-log";
    fsType = "zfs";
  };

  fileSystems."/var/lib/nzbget-dst" = {
    device = "zroot/local/var-lib-nzbget-dst";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/6241-1BC1";
    fsType = "vfat";
    options = ["noauto" "x-systemd.automount"];
  };

  fileSystems."/boot-fallback" = {
    device = "/dev/disk/by-uuid/6258-01A0";
    fsType = "vfat";
    options = ["noauto" "x-systemd.automount"];
  };

  fileSystems."/media/zstorage/files" = {
    device = "zstorage/files";
    fsType = "zfs";
    neededForBoot = true;
  };

  # Storage related mounts
  fileSystems."/media/zstorage/files/audio" = {
    device = "zstorage/files/audio";
    fsType = "zfs";
    options = ["noauto" "x-systemd.automount"];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/ebooks" = {
    device = "zstorage/files/ebooks";
    fsType = "zfs";
    options = ["noauto" "x-systemd.automount"];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/software" = {
    device = "zstorage/files/software";
    fsType = "zfs";
    options = ["noauto" "x-systemd.automount"];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/upload" = {
    device = "zstorage/files/upload";
    fsType = "zfs";
    options = ["noauto" "x-systemd.automount"];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/video" = {
    device = "zstorage/files/video";
    fsType = "zfs";
    options = ["noauto" "x-systemd.automount"];
    #noCheck = true;
  };

  # Persistence of certain hosts paths and home directory paths.
  etu.base.zfs.user.directories = [
    "backups"
  ];

  # Persistence of certificates for nginx
  etu.base.zfs.system.directories = [
    "/var/lib/acme"
  ];

  # Swap devices.
  swapDevices = [];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 8;

  # Set CPU Frequency Governor.
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
