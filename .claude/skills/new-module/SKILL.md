---
name: new-module
description: Use this skill when the user asks to add a new NixOS module, create a module for an application, or install a new package with persistent state in this nixos config repository.
argument-hint: <module-name> [category]
allowed-tools: [Read, Write, Edit, Glob, Grep, Bash]
---

# New NixOS Module

The user wants to add a new module to this NixOS config. Arguments: $ARGUMENTS

## Overview of This Repository's Module System

Modules live under `modules/nixos/<category>/`. Each module is a directory with a `default.nix`. The root module at `modules/nixos/default.nix` imports all category directories, and each category's `default.nix` imports its sub-modules.

**Categories:**
- `base/` — always imported; most modules are enabled by default
- `development/` — development tooling, each module has its own enable option
- `graphical/` — only the font options read by `homeModules.emacs`; there is no NixOS desktop any more
- `services/` — service configurations

The NixOS hosts are servers and a console-only live ISO. A desktop
application for the MacBook belongs in a nix-darwin module under
`modules/darwin/` instead (see *nix-darwin hosts* in AGENTS.md).

## Standard Module Template

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.<category>.<module-name>.enable =
    lib.mkEnableOption "Enable <description>";

  config = lib.mkIf config.etu.<category>.<module-name>.enable {
    # Install packages via home-manager user packages
    etu.user.extraUserPackages = [
      pkgs.<package-name>
    ];

    # Persist config directories (backed up via ZFS safe dataset)
    etu.base.zfs.user.directories = [
      ".config/<app-name>"
    ];

    # Persist local/cache directories (not backed up, ZFS local dataset)
    # etu.base.zfs.localUser.directories = [
    #   ".cache/<app-name>"
    # ];

    # Persist specific files instead of whole directories
    # etu.base.zfs.user.files = [
    #   ".config/<app-name>/settings.ini"
    # ];
  };
}
```

## Persistence Scopes

| Option | Mount | Snapshotted | Use for |
|--------|-------|-------------|---------|
| `etu.base.zfs.user.directories` | `/data` | Yes | Config, important data |
| `etu.base.zfs.user.files` | `/data` | Yes | Single config files |
| `etu.base.zfs.localUser.directories` | `/data/local` | No | Caches, ephemeral state |
| `etu.base.zfs.system.directories` | `/data` | Yes | System-level dirs |
| `etu.base.zfs.local.directories` | `/data/local` | No | System-level local dirs |

Root is tmpfs — nothing persists unless declared here.

## Steps to Add a New Module

1. **Create the module file** at `modules/nixos/<category>/<module-name>/default.nix`
2. **Add it to the category's imports** in `modules/nixos/<category>/default.nix`
3. **Enable it on the desired hosts** in `hosts/<hostname>/configuration.nix`

## Finding the Right Package Name

Run `nix search nixpkgs <keyword>` in the terminal to find the exact package attribute name.

## Common Patterns

### Simple app (most common case)
```nix
{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.<category>.<name>.enable = lib.mkEnableOption "Enable <name>";

  config = lib.mkIf config.etu.<category>.<name>.enable {
    etu.user.extraUserPackages = [ pkgs.<name> ];

    etu.base.zfs.user.directories = [
      ".config/<name>"
    ];
  };
}
```

### App needing extra user groups
```nix
config = lib.mkIf config.etu.<category>.<name>.enable {
  etu.user.extraUserPackages = [ pkgs.<name> ];
  etu.user.extraGroups = [ "dialout" ];  # e.g., for serial/USB devices
  etu.base.zfs.user.directories = [ ".config/<name>" ];
};
```

### App with both config and cache persistence
```nix
config = lib.mkIf ... {
  etu.user.extraUserPackages = [ pkgs.<name> ];
  etu.base.zfs.user.directories = [ ".config/<name>" ];
  etu.base.zfs.localUser.directories = [ ".cache/<name>" ];
};
```

### System-level service with persistence
```nix
config = lib.mkIf ... {
  services.<name>.enable = true;
  etu.base.zfs.system.directories = [ "/var/lib/<name>" ];
};
```

## Hosts

The NixOS hosts are:
- `hosts/server-main-elis/configuration.nix`
- `hosts/server-sparv/configuration.nix`
- `hosts/vps06/configuration.nix`
- `hosts/live-iso/configuration.nix`

Enable options are set directly in the `etu = { ... }` block of each host's configuration.nix.

## Example: Adding a Development Tool

```
modules/nixos/development/<name>/default.nix  ← new module
modules/nixos/development/default.nix         ← added ./<name> to imports
hosts/<host>/configuration.nix                ← added development.<name>.enable = true
```
