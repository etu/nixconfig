{ pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    kdeApplications.spectacle
    okular
  ];

  # Enable the Plasma Desktop Environment.
  services.xserver.desktopManager.plasma5.enable = true;

  # Workaround for missing settings in kde settings panel:
  # https://github.com/NixOS/nixpkgs/issues/27050#issuecomment-315324541
  environment.variables.QT_PLUGIN_PATH = [ "${pkgs.plasma-desktop}/lib/qt-5.9/plugins/kcms" ];

  # Enable autologin.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.autoLogin.enable = true;
  services.xserver.displayManager.sddm.autoLogin.user = "etu";
}
