{ config, pkgs, ... }:
let
  hpkgs = import
    (builtins.fetchTarball {
      url = https://github.com/NixOS/nixpkgs-channels/archive/b0bbacb52134a7e731e549f4c0a7a2a39ca6b481.tar.gz;
    }) { };
in
{
  # Make sure to have NGiNX enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."hass.lan".locations."/" = {
    proxyWebsockets = true;
    proxyPass = "http://127.0.0.1:8123";
    extraConfig = ''
      allow 10.3.0.0/24;
      deny all;
    '';
  };

  # Enable Home Assistant, open port and add the hass user to the dialout group
  services.home-assistant.enable = true;
  services.home-assistant.autoExtraComponents = false;
  services.home-assistant.package = pkgs.home-assistant.override {
    extraComponents = [
      "cast"
      "discovery"
      "esphome"
      "hue"
      "kodi"
      "media_player"
      "mobile_app"
      "notify"
      "system_health"
      "yr"
      "zha"
      "zwave"
    ];
  };

  users.users.hass.extraGroups = [ "dialout" ];

  # Bind mount home assistants files to have persistence of hass configs
  fileSystems."/var/lib/hass" = {
    device = "/persistent/var/lib/hass";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };
}
