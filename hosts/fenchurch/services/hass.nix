{ config, pkgs, ... }:

let
  hpkgs = import (builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/b0bbacb52134a7e731e549f4c0a7a2a39ca6b481.tar.gz;
  }) {};
in {
  # Make sure to have NGiNX enabled
  services.nginx.enable = true;
  services.nginx.virtualHosts."hass.lan" = {
    locations."/".extraConfig = ''
      allow 10.3.0.0/24;
      deny all;

      proxy_pass http://127.0.0.1:8123;
      proxy_http_version 1.1;
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;
    '';
  };

  # Enable Home Assistant, open port and add the hass user to the dialout group
  services.home-assistant.enable = true;
  services.home-assistant.autoExtraComponents = false;
  services.home-assistant.package = hpkgs.home-assistant.override {
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
