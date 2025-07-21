{
  config,
  lib,
  ...
}: {
  options.etu.games.steam.enable = lib.mkEnableOption "Enable games steam settings";

  config = lib.mkIf config.etu.games.steam.enable {
    # Allow to install some unfree packages.
    etu.base.nix.allowUnfree = [
      "steam"
      "steam-unwrapped"
    ];

    # Enable steam settings.
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    };

    # Enable steam hardware support.
    hardware.steam-hardware.enable = true;

    # Enable persistence for steam files.
    etu.base.zfs.localUser.directories = [
      ".steam"
      ".config/unity3d/" # To remember settings for Valheim
      ".local/share/Steam"
    ];
  };
}
