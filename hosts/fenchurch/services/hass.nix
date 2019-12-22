{ config, pkgs, ... }:

{
  # Enable Home Assistant, open port and add the hass user to the dialout group
  services.home-assistant.enable = true;
  services.home-assistant.openFirewall = true;
  services.home-assistant.autoExtraComponents = false;
  services.home-assistant.package = pkgs.home-assistant.override {
    extraComponents = [
      "cast"
      "discovery"
      "esphome"
      "hue"
      "kodi"
      "media_player"
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
