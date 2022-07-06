{ config, lib, pkgs, ... }:

{
  options.etu.graphical.sway = {
    enable = lib.mkEnableOption "Enables sway and auto login for my user";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.sway;
      description = "Which sway package to use";
      readOnly = true;
    };
    lockCommand = lib.mkOption {
      type = lib.types.str;
      default = "${pkgs.swaylock}/bin/swaylock -f -k -i ${config.etu.graphical.sway.wallpaperPackage}/dark.jpg";
      description = "Lock screen command";
    };
    xkbFile = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ./xkb-keymap.nix { };
    };
    wallpaperPackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ./wallpaper.nix { };
      description = "Which wallpaper package to use";
    };
  };
}
