# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
#
# Initial deployment:
# 1. Put filesystem password in /tmp/secret.key
# 2. Deploy it from a remote system
#    $ nix run github:numtide/nixos-anywhere -- --flake .#laptop-private-elis root@10.69.0.55
# 3. Reboot back into install system
# 4. sudo zpool import -f zroot
# 5. sudo zfs load-key -a -L prompt
# 6. sudo zfs change-key -o keylocation=prompt zroot
#
# Later deployments:
# $ deploy --skip-checks --targets .#laptop-private-elis
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
    stateVersion = "24.05";

    base.fish.enableUserZoxideCd = true;
    development.enable = true;
    development.flipper-zero.enable = true;
    games.enable = false;
    games.minecraft.enable = true;
    games.wowup.enable = true;
    games.steam.enable = true;
    games.steam-controller.enable = true;
    graphical.enable = true;
    graphical.sway.enable = true;
    graphical.fdm-printing.enable = true;
    graphical.hamradio.enable = true;
    graphical.signal.enable = true;
    graphical.flatpak.enablePersistence = true;
    services.netdata.enable = true;
    theme.enable = true;
    user.enable = true;
    user.extraGroups = ["video" "docker" "libvirtd"];

    # Allow home fileserver to connect to fetch snapshots.
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.syncoid.server-main-elis;

    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
      "zroot/safe/home".use_template = ["home"];
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
      hostName = "aarch64.nixos.community";
      maxJobs = 64;
      sshKey = config.age.secrets."etu@aarch64.nixos.community".path;
      sshUser = "etu";
      system = "aarch64-linux";
      supportedFeatures = ["big-parallel"];
    }
  ];
}
