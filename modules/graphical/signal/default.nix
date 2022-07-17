{ config, lib, pkgs, ... }:

{
  options.etu.graphical.signal.enable = lib.mkEnableOption "Enable graphical signal settings";

  config = lib.mkIf config.etu.graphical.signal.enable {
    # Install signal for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [ pkgs.signal-desktop ];
    };

    # Enable persistence for signal files.
    etu.base.zfs.user.directories = [
      ".config/Signal"
    ];
  };
}
