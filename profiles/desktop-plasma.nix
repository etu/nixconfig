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

  # Enable autologin.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.autoLogin.enable = true;
  services.xserver.displayManager.sddm.autoLogin.user = "etu";
}
