{
  config,
  lib,
  ...
}:
let
  pubkeys = import ../../../../pubkeys.nix;
in
{
  options.etu.base.nix.enable = lib.mkEnableOption "Enable base nix settings";
  options.etu.base.nix.allowUnfree = lib.mkOption {
    default = [ ];
    type = lib.types.listOf lib.types.str;
    description = "Enable unfree packages. Applies to home-manager too, since home-manager.useGlobalPkgs shares this pkgs instance.";
  };
  options.etu.base.nix.remoteBuilds = lib.mkOption {
    default = false;
    type = lib.types.bool;
    description = "Whether Nix may build x86_64-linux derivations on the servers (nix.buildMachines) from this Mac, so the NixOS hosts can be built and deployed here without a local Linux builder. Requires the passphrase-less builder key (see `builders` in pubkeys.nix) to exist, so enable it per host.";
  };

  config = lib.mkIf config.etu.base.nix.enable {
    # If we allow certain unfree packages, enable the nix option to do so.
    nixpkgs.config.allowUnfreePredicate =
      pkg: builtins.elem (lib.getName pkg) config.etu.base.nix.allowUnfree;

    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
    nix.settings.trusted-users = [
      "root"
      config.etu.user.username
    ];

    # Build the NixOS hosts (x86_64-linux) on the home server from this Mac.
    # Nix has to keep its default `builders = @/etc/nix/machines`: nix-darwin
    # only writes that file while distributed builds are on, and clears
    # `builders` when they are off. The builder key is passphrase-less (Nix
    # can neither prompt for one nor use the ssh-agent) and is authorized for
    # root on the builder, see `builders` in pubkeys.nix.
    nix.distributedBuilds = config.etu.base.nix.remoteBuilds;
    nix.buildMachines = lib.mkIf config.etu.base.nix.remoteBuilds [
      {
        hostName = "server-main-elis";
        sshUser = "root";
        sshKey = "${config.users.users.${config.etu.user.username}.home}/.ssh/nix-builder";
        protocol = "ssh-ng";
        systems = [ "x86_64-linux" ];
        maxJobs = 8;
        supportedFeatures = [
          "kvm"
          "benchmark"
          "big-parallel"
        ];
        publicHostKey = pubkeys.builderHostKeys.server-main-elis;
      }
    ];

    # Let the remote builder fetch substitutes instead of only receiving the
    # inputs copied over from here.
    nix.settings.builders-use-substitutes = lib.mkIf config.etu.base.nix.remoteBuilds true;

    # Clean up the Nix store like on the NixOS servers: collect garbage older
    # than a week once a week (nix-darwin's default interval, Sundays 03:15)
    # and deduplicate the store daily. Missed runs while asleep run on wake.
    nix.gc.automatic = true;
    nix.gc.options = "--delete-older-than 7d";
    nix.optimise.automatic = true;
    nix.optimise.interval = [
      {
        Hour = 4;
        Minute = 15;
      }
    ];

    # Extra binary caches
    nix.settings.substituters = [
      "https://nix-community.cachix.org"
      "https://etu.cachix.org"
    ];
    nix.settings.trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "etu.cachix.org-1:CeyfbMJZHZ95TScp8+I8+EeyzbncqPSj1xfCK9vOAFE="
    ];
  };
}
