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
    mpv
    kdeconnect
    firefox
    okular

    # Require unfree
    spotify
  ];

  # Enable pulseaudio.
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Keyboard layout.
  services.xserver.layout = "se";
  services.xserver.xkbOptions = "eurosign:e,ctrl:nocaps,numpad:mac,kpdl:dot";
  services.xserver.xkbVariant = "dvorak";

  # Enable the Plasma Desktop Environment.
  services.xserver.desktopManager.plasma5.enable = true;

  # Workaround for missing settings in kde settings panel:
  # https://github.com/NixOS/nixpkgs/issues/27050#issuecomment-315324541
  environment.variables.QT_PLUGIN_PATH = [ "${pkgs.plasma-desktop}/lib/qt-5.9/plugins/kcms" ];

  # Enable autologin.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.autoLogin.enable = true;
  services.xserver.displayManager.sddm.autoLogin.user = "etu";

  # Don't install xterm.
  services.xserver.desktopManager.xterm.enable = false;

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
