{ config, lib, pkgs, ... }:

{
  options.etu.graphical.telegram.enable = lib.mkEnableOption "Enable graphical telegram settings";

  config = lib.mkIf config.etu.graphical.telegram.enable {
    # Install the telegram chat client using home manager.
    etu.user.extraUserPackages = [ pkgs.tdesktop ];

    # Enable persistence for telegram files.
    etu.base.zfs.user.directories = [
      ".local/share/TelegramDesktop/tdata"
    ];
  };
}
