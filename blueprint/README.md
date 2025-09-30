# [WIP] branch to migrate to blueprint

> [!CAUTION]
> Instructions for use: do not.

Currently builds like this:
```sh
nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.desktop-elis.config.system.build.toplevel --option allow-import-from-derivation true --impure
```

Can be nix flake shown like this:
```sh
nix flake show --option allow-import-from-derivation true --impure
```

## TODO

- [X] Fix overlays
- [X] Work around many hacks with custom packages
- [X] Work around many hacks with custom modules
- [X] Work around hacks with custom arguments to functions
- [X] Work around hacks with data variables passing
- [X] Look into how to use deploy-rs with blueprint
- [X] Look into how to expose my iso as a package
- [ ] Look into how to use deploy-rs checks with blueprint
- [ ] Configure `allow-import-from-derivation` in nix config
- [ ] When all is done, we can probably drop in replace the `flake.nix`, the `.gitignore`, the `.envrc` in the parent folder and drop the symlinks here.
