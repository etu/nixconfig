{ config, pkgs, ... }:

let
  hassPkgs = import (builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/1036dc664169b32613ec11b58cc1740c7511a340.tar.gz;
  }) {};
in {
  # Enable Home Assistant, open port and add the hass user to the dialout group
  services.home-assistant.enable = true;
  services.home-assistant.openFirewall = true;
  services.home-assistant.autoExtraComponents = false;
  services.home-assistant.package = hassPkgs.home-assistant.override {
    extraComponents = [
      "cast"
      "discovery"
      "hue"
      "kodi"
      "media_player"
      "notify"
      "system_health"
      "yr"
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
