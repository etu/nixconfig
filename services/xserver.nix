{ pkgs, ... }:

{
  # Enable the smartcard deamon.
  services.pcscd.enable = true;

  # Enable some font thingy.
  fonts.fontconfig.ultimate.enable = true;

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

  # Enable autologin.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.autoLogin.enable = true;
  services.xserver.displayManager.sddm.autoLogin.user = "etu";

  # Enable networkmanager.
  networking.networkmanager.enable = true;

  # Enable firewall.
  networking.firewall.enable = true;

  # 1714-1764 is KDE Connect.
  networking.firewall.allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
  networking.firewall.allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
}
