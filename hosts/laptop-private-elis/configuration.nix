# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  myData,
  pkgs,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix
  ];

  # Set hostname
  networking.hostName = "laptop-private-elis";

  # Settings needed for ZFS
  networking.hostId = "27416952";

  # My module settings
  etu = {
    stateVersion = "22.11";

    development.enable = true;
    development.flipper-zero.enable = true;
    games.enable = false;
    games.minecraft.enable = true;
    graphical.enable = true;
    graphical.fdm-printing.enable = true;
    graphical.hamradio.enable = true;
    graphical.signal.enable = true;
    services.syncthing.enable = true;
    services.netdata.enable = true;
    user.enable = true;
    user.extraGroups = ["video" "docker" "libvirtd"];

    # Install tree-sitter mode in Emacs on this computer.
    base.emacs.extraConfig = [
      ''
        ;; Install tree-sitter to get more consistent highlight of all
        ;; sorts of different things.
        (use-package tree-sitter
          :ensure t
          :config
          (global-tree-sitter-mode)
          (add-hook 'tree-sitter-mode-hook 'tree-sitter-hl-mode))

        (use-package tree-sitter-langs :ensure t)
      ''
    ];

    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
      "zroot/safe/home".use_template = ["home"];
    };
    # Enable syncing of some filesystems
    base.syncoid.enable = true;
    base.syncoid.commands = {
      "zroot/safe/data".target = "root@home.elis.nu:zroot/backups/current/laptop-private-elis/data";
      "zroot/safe/home".target = "root@home.elis.nu:zroot/backups/current/laptop-private-elis/home";
    };
  };

  # Set up docker
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [virt-manager];
  virtualisation.spiceUSBRedirection.enable = true;

  # Enable blueman.
  services.blueman.enable = true;

  # Add community server to known hosts
  programs.ssh.knownHosts."aarch64.nixos.community".publicKey = myData.pubkeys.systems."aarch64.nixos.community";

  age.secrets = {
    inherit (myData.ageModules) "etu@aarch64.nixos.community" "etu@aarch64.nixos.community.pub";
    inherit (myData.ageModules) syncoid-workstations-ssh-ec;
  };

  # Set up remote builds
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "home.elis.nu";
      maxJobs = 8;
      sshKey = config.age.secrets.syncoid-workstations-ssh-ec.path;
      sshUser = "root";
      system = "x86_64-linux";
    }
    {
      hostName = "aarch64.nixos.community";
      maxJobs = 64;
      sshKey = config.age.secrets."etu@aarch64.nixos.community".path;
      sshUser = "etu";
      system = "aarch64-linux";
      supportedFeatures = ["big-parallel"];
    }
  ];
}
