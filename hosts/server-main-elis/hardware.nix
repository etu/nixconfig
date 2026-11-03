{
  config,
  pkgs,
  lib,
  modulesPath,
  inputs,
  ...
}:
{
  imports = [
    # Hardware settings
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-gpu-amd

    # Scanned modules
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Configure boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.extraInstallCommands = ''
    mkdir -p /boot/EFI/Microsoft/Boot/ /boot-fallback/EFI/Microsoft/Boot/
    cp /boot/EFI/nixos/grubx64.efi /boot/EFI/Microsoft/Boot/bootmgfw.efi
    cp /boot-fallback/EFI/nixos/grubx64.efi /boot-fallback/EFI/Microsoft/Boot/bootmgfw.efi
  '';

  boot.loader.grub.mirroredBoots = [
    {
      devices = [ "/dev/disk/by-uuid/6258-01A0" ];
      path = "/boot-fallback";
    }
  ];

  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # Enable graphics drivers for the graphics card.
  hardware.graphics.enable = true;
  hardware.graphics.extraPackages = [
    # for newer GPUs on NixOS >24.05 or unstable
    pkgs.vpl-gpu-rt
  ];

  # Additonal intel things
  environment.systemPackages = [
    pkgs.pciutils
    pkgs.nvtopPackages.intel
  ];

  age.secrets = {
    inherit (config.etu.data.ageModules) server-main-elis-initrd-sshd;
    inherit (config.etu.data.ageModules) server-main-elis-initrd-tailscaled-state;
    inherit (config.etu.data.ageModules) syncoid-server-main-elis-ssh-ec;
  };

  # Remote unlocking of encrypted ZFS
  boot.initrd = {
    availableKernelModules = [
      "xhci_pci"
      "ahci"
      "usbhid"
      "usb_storage"
      "sd_mod"
      "nvme"
      "thunderbolt"
    ];
    kernelModules = [
      "kvm-amd"
      "r8169"
    ];
    network.enable = true;
    # Listen to ssh to let me decrypt zfs
    network.ssh = {
      enable = true;
      port = 2222;
      hostKeys = [
        config.etu.data.ageModules.server-main-elis-initrd-sshd.path
      ];
      authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;
    };
  };

  # Join the tailnet from the initrd as its own node,
  # server-main-elis-initrd, so the ZFS unlock over SSH (port 2222)
  # also works from outside the LAN.
  #
  # The node was registered once with a separate tailscaled (tagged
  # tag:initrd, so key expiry is off) and its tailscaled.state is the
  # server-main-elis-initrd-tailscaled-state agenix secret. Like the
  # initrd SSH host key it is copied into the initrd when the bootloader
  # is installed, so it is readable from the boot partition: keep the
  # tag's ACLs limited to incoming SSH.
  #
  # Userspace networking needs no tun device or firewall rules and
  # forwards incoming tailnet connections to the initrd sshd. Only the
  # Go binary is copied, the package's wrapper would pull iptables,
  # iproute2 and friends into the initrd.
  boot.initrd.secrets."/var/lib/tailscale/tailscaled.state" =
    config.etu.data.ageModules.server-main-elis-initrd-tailscaled-state.path;

  # Store paths only referenced from a unit aren't copied into the
  # initrd, so the binary goes in through extraBin. It's a multicall
  # binary: called as tailscale it is the CLI, for debugging from the
  # initrd shell.
  boot.initrd.systemd.extraBin =
    let
      tailscaled = pkgs.runCommand "tailscaled-initrd" { } ''
        install -Dm755 ${pkgs.tailscale}/bin/.tailscaled-wrapped $out/bin/tailscaled
      '';
    in
    {
      tailscaled = "${tailscaled}/bin/tailscaled";
      tailscale = "${tailscaled}/bin/tailscaled";
    };

  boot.initrd.systemd.services.tailscaled = {
    description = "Tailscale node agent in the initrd";
    wantedBy = [ "initrd.target" ];
    after = [ "network.target" ];
    # Stop before switching root, so the real tailscaled starts cleanly.
    conflicts = [ "initrd-switch-root.target" ];
    before = [ "initrd-switch-root.target" ];
    unitConfig.DefaultDependencies = false;
    serviceConfig = {
      ExecStart = lib.concatStringsSep " " [
        "/bin/tailscaled"
        "--tun=userspace-networking"
        "--statedir=/var/lib/tailscale"
        "--state=/var/lib/tailscale/tailscaled.state"
      ];
      Restart = "on-failure";
    };
  };

  # Prompt me for password to decrypt zfs via SSH
  #
  # SSH in on port 2222, shell sources /var/empty/.profile which runs:
  #   systemd-tty-ask-password-agent --watch
  # This intercepts pending systemd password requests (from
  # zfs-load-key@pool.service units) and routes them to the SSH
  # terminal. --watch stays running to catch each pool as it becomes
  # pending. systemd continues naturally once all keys are provided.
  boot.initrd.systemd.services.zfs-setup-root-profile = {
    description = "Prepare root .profile for ZFS unlocking via SSH";
    wantedBy = [ "initrd.target" ];
    before = [ "initrd-root-fs.target" ];
    unitConfig.DefaultDependencies = false;
    script = ''
      mkdir -p /var/empty
      echo "systemd-tty-ask-password-agent --watch" > /var/empty/.profile
    '';
    serviceConfig.Type = "oneshot";
  };

  # Roll back certain filesystems to empty state on boot
  boot.initrd.systemd.services.rollback-nzbget-dst = {
    description = "Rollback nzbget-dst ZFS snapshot to empty state";
    wantedBy = [ "initrd.target" ];
    after = [ "zfs-import-zroot.service" ];
    before = [ "sysroot.mount" ];
    unitConfig.DefaultDependencies = false;
    path = [ pkgs.zfs ];
    script = ''
      zfs rollback -r zroot/local/var-lib-nzbget-dst@empty
    '';
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
  };

  # Enable ZFS.
  boot.supportedFilesystems = [ "zfs" ];

  # Enable ZFS scrubbing.
  services.zfs.autoScrub.enable = true;

  # Filesystem mounts.
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [
      "defaults"
      "size=10G"
      "mode=755"
    ];
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

  fileSystems.${config.etu.localPrefix} = {
    device = "zroot/local/data";
    fsType = "zfs";
    neededForBoot = true;
    options = [
      "defaults"
      "noexec"
    ];
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
    options = [
      "noauto"
      "x-systemd.automount"
    ];
  };

  fileSystems."/boot-fallback" = {
    device = "/dev/disk/by-uuid/6258-01A0";
    fsType = "vfat";
    options = [
      "noauto"
      "x-systemd.automount"
    ];
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
    options = [
      "noauto"
      "x-systemd.automount"
    ];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/ebooks" = {
    device = "zstorage/files/ebooks";
    fsType = "zfs";
    options = [
      "noauto"
      "x-systemd.automount"
    ];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/software" = {
    device = "zstorage/files/software";
    fsType = "zfs";
    options = [
      "noauto"
      "x-systemd.automount"
    ];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/video" = {
    device = "zstorage/files/video";
    fsType = "zfs";
    options = [
      "noauto"
      "x-systemd.automount"
    ];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/video/movies" = {
    device = "zstorage/files/video/movies";
    fsType = "zfs";
    options = [
      "noauto"
      "x-systemd.automount"
    ];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/video/movies.sv" = {
    device = "zstorage/files/video/movies.sv";
    fsType = "zfs";
    options = [
      "noauto"
      "x-systemd.automount"
    ];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/video/series" = {
    device = "zstorage/files/video/series";
    fsType = "zfs";
    options = [
      "noauto"
      "x-systemd.automount"
    ];
    #noCheck = true;
  };

  fileSystems."/media/zstorage/files/video/series.sv" = {
    device = "zstorage/files/video/series.sv";
    fsType = "zfs";
    options = [
      "noauto"
      "x-systemd.automount"
    ];
    #noCheck = true;
  };

  # Persistence of certain hosts paths and home directory paths.
  etu.base.zfs.root.directories = [
    "backups"
  ];

  # Persistence of certificates for nginx
  etu.base.zfs.system.directories = [
    "/var/lib/acme"
  ];

  # Swap devices.
  swapDevices = [ ];

  # Set max jobs in nix.
  nix.settings.max-jobs = lib.mkDefault 8;

  # Use the host platform for building by default to avoid cross compiling.
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # Set CPU Frequency Governor.
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
