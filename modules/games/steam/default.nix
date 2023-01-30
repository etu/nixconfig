{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.games.steam.enable = lib.mkEnableOption "Enable games steam settings";

  config = lib.mkIf config.etu.games.steam.enable {
    # Allow to install some unfree packages.
    etu.base.nix.allowUnfree = [
      "steam"
      "steam-original"
      "steam-runtime"
    ];

    # Enable 32bit libs for steam and such.
    hardware.opengl = {
      driSupport = true;
      driSupport32Bit = true;
    };

    # Support 32bit audio things
    services.pipewire.alsa.support32Bit = true;

    # Enable udev rules for steam controller
    services.udev.packages = [
      pkgs.sc-controller
    ];

    # Steam link ports
    networking.firewall.allowedTCPPorts = [27036 27037];
    networking.firewall.allowedUDPPorts = [27031 27036];

    # Install steam using home manager.
    etu.user.extraUserPackages = [pkgs.steam pkgs.sc-controller];

    # Enable persistence for steam files.
    etu.base.zfs.user.directories = [
      ".steam"
      ".local/share/Steam"
    ];
  };
}
