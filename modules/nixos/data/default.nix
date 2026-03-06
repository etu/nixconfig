{ lib, ... }:
let
  registry = import ../../../secrets-registry.nix;

  # Strip the hostKeys field before passing entries to the agenix NixOS module.
  toAgeModule = _name: secret: builtins.removeAttrs secret [ "hostKeys" ];
in
{
  options.etu.data = lib.mkOption {
    default = {
      ageModules = builtins.mapAttrs toAgeModule registry;
      pubkeys = import ../../../pubkeys.nix;
    };
  };
}
