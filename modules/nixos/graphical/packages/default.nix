{ pkgs, ... }:
{
  # Install packages using home-manager for graphical systems.
  etu.user.extraUserPackages = [
    pkgs.chromium # Chromium browser
    pkgs.feh # Image display tool
    pkgs.imv # Image display tool
    pkgs.mpv # Media player
    pkgs.pavucontrol # Pulse audio volume control
    pkgs.sshfs-fuse # SSHFS client
    pkgs.stupidterm # Another terminal emulator
    pkgs.yt-dlp # YouTube download client
    pkgs.android-tools # ADB & Fastboot
  ];
}
