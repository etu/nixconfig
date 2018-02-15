{ pkgs, ... }:

{
  # Enable the smartcard deamon.
  services.pcscd.enable = true;

  # Sane font defaults
  fonts.enableFontDir = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fontconfig.cache32Bit = true;

  fonts.fontconfig.ultimate.enable = true;
  fonts.fontconfig.ultimate.preset = "osx";

  fonts.fonts = with pkgs; [
    liberation_ttf
  ];

  # Used for nvidia drivers, spotify and steam and such
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    firefox-bin
    firejail
    kdeconnect
    mpv
    stupidterm

    # Keysigning party
    signing-party
    msmtp

    # Fonts
    emacs-all-the-icons-fonts
    powerline-fonts
  ];

  security.wrappers = {
    firejail.source = "${pkgs.firejail.out}/bin/firejail";
  };

  # Enable pulseaudio.
  hardware.pulseaudio.enable = true;

  # Install keybase
  services.keybase.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Don't have xterm as a session manager.
  services.xserver.desktopManager.xterm.enable = false;

  # Keyboard layout.
  services.xserver.layout = "se";
  services.xserver.xkbOptions = "eurosign:e,ctrl:nocaps,numpad:mac,kpdl:dot";
  services.xserver.xkbVariant = "dvorak";

  # Enable networkmanager.
  networking.networkmanager.enable = true;

  # 1714-1764 is KDE Connect.
  networking.firewall.allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
  networking.firewall.allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];

  # 8000 is for random web sharing things.
  networking.firewall.allowedTCPPorts = [ 8000 ];

  # Define extra groups for user.
  users.extraUsers.etu.extraGroups = [ "networkmanager" "dialout" ];
}
