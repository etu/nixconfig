{ pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    evince
    pavucontrol
    gnome3.gnome-tweak-tool
    gnomeExtensions.dash-to-dock
    gnomeExtensions.topicons-plus
  ];

  # Ugly hack to get gdm and gnome to understand keyboard settings
  # https://github.com/NixOS/nixpkgs/issues/14318#issuecomment-309250231
  environment.etc."X11/xorg.conf.d/00-keyboard.conf".text = ''
    Section "InputClass"
      Identifier "Keyboard catchall"
      MatchIsKeyboard "on"
      Option "XkbRules" "base"
      Option "XkbModel" "pc105"
      Option "XkbLayout" "se"
      Option "XkbOptions" "eurosign:e,ctrl:nocaps,numpad:mac,kpdl:dot"
      Option "XkbVariant" "dvorak"
    EndSection
  '';

  # Another ugly hack to get gdm and gnome to understand keyboard settings
  # https://github.com/NixOS/nixpkgs/issues/14318#issuecomment-330193990
  services.xserver.desktopManager.gnome3.extraGSettingsOverrides = ''
    [org.gnome.desktop.input-sources]
    sources=[('xkb', 'se+dvorak')]
    xkb-options=['eurosign:e', 'ctrl:nocaps', 'numpad:mac', 'kpdl:dot']
  '';

  # Enable the Plasma Desktop Environment.
  services.xserver.desktopManager.gnome3.enable = true;

  # Enable autologin.
  services.xserver.displayManager.sddm.enable = false;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.autoLogin.enable = true;
  services.xserver.displayManager.gdm.autoLogin.user = "etu";
}
