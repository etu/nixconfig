{ config, lib, pkgs, ... }:

{
  imports = [
    ./evolution
    ./firefox
    ./gnupg
    ./spotify
    ./sway
    ./telegram
    ./terminal
    ./theme
    ./virtualbox
  ];

  options.etu.graphical.enable = lib.mkEnableOption "Enable graphical settings";

  config = lib.mkIf config.etu.graphical.enable {
    etu = {
      graphical.evolution.enable = true;
      graphical.firefox.enable = true;
      graphical.gnupg.enable = true;
      graphical.sway.enable = true;
      graphical.telegram.enable = true;
      graphical.terminal.enable = true;
      graphical.theme.enable = true;
    };

    # Install some comand line tools I cummonly want available for my
    # home directory on graphical systems.
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [
        pkgs.feh         # Image display tool
        pkgs.mpv         # Media player
        pkgs.pavucontrol # Pulse audio volume control
        pkgs.sshfs-fuse  # SSHFS client
        pkgs.yt-dlp      # Youtube download client
      ];

      home.file = {
        # Mpv config file - Don't show images embedded in music files
        ".config/mpv/mpv.conf".text = "no-audio-display";
      };
    };
  };
}
