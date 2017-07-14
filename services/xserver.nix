{ pkgs, ... }:

{
  # Enable some font thingy
  fonts.fontconfig.ultimate.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Keyboard layout
  services.xserver.layout = "se";
  services.xserver.xkbOptions = "eurosign:e,ctrl:nocaps,numpad:mac,kpdl:dot";
  services.xserver.xkbVariant = "dvorak";

  # Enable the Plasma Desktop Environment.
  services.xserver.desktopManager.plasma5.enable = true;

  # Enable autologin
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.autoLogin.enable = true;
  services.xserver.displayManager.sddm.autoLogin.user = "etu";
}
