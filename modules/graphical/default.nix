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

      # Define extra groups for user.
      user.extraGroups = [ "networkmanager" ];

      # Directories to mount persistent for my user on graphical sessions
      base.zfs.user.directories = [
        ".config/pipewire/media-session.d"
        "Downloads"
        "code"
        "documents"
        "org"
      ];
    };

    # Enable networkmanager.
    networking.networkmanager.enable = true;
    networking.networkmanager.wifi.backend = "iwd";

    # 8000 is for random web sharing things.
    networking.firewall.allowedTCPPorts = [ 8000 ];

    # Install some comand line tools I cummonly want available for my
    # home directory on graphical systems.
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [
        pkgs.feh         # Image display tool
        pkgs.mpv         # Media player
        pkgs.pavucontrol # Pulse audio volume control
        pkgs.sshfs-fuse  # SSHFS client
        pkgs.yt-dlp      # Youtube download client
        pkgs.chromium    # Chromium browser
        pkgs.stupidterm  # Another terminal emulator
      ];

      home.file = {
        # Mpv config file - Don't show images embedded in music files
        ".config/mpv/mpv.conf".text = "no-audio-display";
      };
    };

    # Persistence of certain hosts paths for graphical systems.
    etu.base.zfs.system.directories = [
      "/etc/nixos"
      "/etc/NetworkManager/system-connections"
    ];
  };
}
