{ config, lib, ... }:

let
  unfreePackages = (
    (lib.optionals config.my.emacs.enable [
      "intelephense"
    ]) ++ (lib.optionals config.my.gaming.enable [
      "minecraft-launcher"
      "steam"
      "steam-original"
      "steam-runtime"
    ])
  );
in
{
  config = lib.mkIf ((builtins.length unfreePackages) > 0) {
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
}
