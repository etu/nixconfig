# Derived from secrets-registry.nix.  Do not edit directly.
# Consumed by the agenix CLI to determine which public keys may decrypt each
# secret file.
let
  registry = import ./secrets-registry.nix;

  # Convert a Nix store path (e.g. /etc/nixos/secrets/foo.age) to a
  # repo-relative string ("secrets/foo.age") as required by the agenix CLI.
  repoRoot = toString ./.;
  relPath = p: builtins.replaceStrings [ "${repoRoot}/" ] [ "" ] (toString p);
in
builtins.listToAttrs (
  map (name: {
    name = relPath registry.${name}.file;
    value = {
      publicKeys = registry.${name}.hostKeys;
    };
  }) (builtins.attrNames registry)
)
