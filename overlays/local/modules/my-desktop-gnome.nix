{ config, lib, pkgs, ... }:

with lib;

let
 cfg = config.my.desktop-gnome;

in {
  options = {
    my.desktop-gnome = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enables gnome desktop with some settings and auto login on my user
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # Enable autologin through GDM.
    services.xserver.displayManager.sddm.enable = false;
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.displayManager.gdm.autoLogin.enable = true;
    # TODO: read my.user.username in some way
    services.xserver.displayManager.gdm.autoLogin.user = "etu";

    # Temporary fix so gnome doesn't crash on nixos-rebuild switches. This can
    # be removed whenever it's fixed upstream:
    # https://github.com/NixOS/nixpkgs/pull/45936
    systemd.services.accounts-daemon.restartIfChanged = false;

    # Enable the Gnome Desktop Environment.
    services.xserver.desktopManager.gnome3.enable = true;

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      evince
      pavucontrol
      gnome3.gnome-tweak-tool
      gnomeExtensions.dash-to-dock
      gnomeExtensions.topicons-plus
    ];

    # Without this the gsettings overrides won't work.
    services.xserver.desktopManager.gnome3.extraGSettingsOverridePackages = with pkgs; [
      gnome3.gnome_shell
    ];

    # Extra gsettings overrides.
    services.xserver.desktopManager.gnome3.extraGSettingsOverrides = ''
      [org.gnome.desktop.input-sources]
      sources=[('xkb', 'se+dvorak')]
      xkb-options=['eurosign:e', 'ctrl:nocaps', 'numpad:mac', 'kpdl:dot']

      [org.gnome.shell]
      always-show-log-out=true

      [org.gnome.desktop.wm.preferences]
      resize-with-right-button=true

      [org.gnome.shell]
      enabled-extensions=['dash-to-dock@micxgx.gmail.com', 'TopIcons@phocean.net', 'alternate-tab@gnome-shell-extensions.gcampax.github.com']
    '';
  };
}
