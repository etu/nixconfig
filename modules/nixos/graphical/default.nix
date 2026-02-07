{
  config,
  lib,
  flake,
  ...
}:
{
  imports = [
    ./fdm-printing
    ./firefox
    ./flatpak
    ./gnupg
    ./hamradio
    ./packages
    ./persistence
    ./sway
    ./terminal
    ./theme
    ./virtualbox
    ./window-managers
    ./xkb-keymap
  ];

  options.etu.graphical.enable = lib.mkEnableOption "Enable graphical settings";

  config = lib.mkIf config.etu.graphical.enable {
    etu = {
      graphical.firefox.enable = lib.mkDefault true;
      graphical.flatpak.enable = lib.mkDefault true;
      graphical.gnupg.enable = lib.mkDefault true;
      graphical.terminal.enable = lib.mkDefault true;
      graphical.theme.enable = lib.mkDefault true;

      # Define extra groups for user.
      user.extraGroups = [
        "networkmanager"
        "adbusers"
      ];
    };

    # Enable gnome keyring (related to ~/.config/goa-1.0 and ~/.local/share/keyrings).
    services.gnome.gnome-keyring.enable = true;

    # Enable networkmanager.
    networking.networkmanager.enable = true;
    networking.networkmanager.wifi.backend = "iwd";
    networking.wireless.iwd.settings.Settings.AutoConnect = true;

    # 8000 is for random web sharing things.
    networking.firewall.allowedTCPPorts = [ 8000 ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.graphical-dotfiles
      ];
    };
  };
}
