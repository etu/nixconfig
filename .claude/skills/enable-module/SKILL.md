---
name: enable-module
description: Use this skill when the user wants to enable an existing NixOS module on one or more hosts in this config repository.
argument-hint: <module.option> [host1] [host2] ...
allowed-tools: [Read, Write, Edit, Glob, Grep, Bash]
---

# Enable NixOS Module on Hosts

The user wants to enable an existing module option on one or more hosts. Arguments: $ARGUMENTS

Parse the arguments:
- First argument: the module option path (e.g., `graphical.audacity`, `development.flipper-zero`, `games.steam`)
- Remaining arguments: host names to enable it on (e.g., `desktop-elis`, `laptop-private-elis`)
  - If no hosts are specified, default to both `desktop-elis` and `laptop-private-elis`

## Host Configuration Files

Each host config lives at `hosts/<hostname>/configuration.nix`. The `etu = { ... }` block is where all module enables are set.

**Available hosts:** desktop-elis, laptop-private-elis, laptop-work-elis, desktop-caroline, laptop-private-caroline, server-main-elis, server-sparv, vps06, live-iso

## Steps

### 1. Verify the module exists

Grep for the option in the module files to confirm it exists and get the exact option path:
```
Grep for: options.etu.<category>.<module>.enable
Path: modules/nixos/
```

If the module doesn't exist, stop and tell the user to use `/new-module` instead.

### 2. Read each target host's configuration.nix

Read `hosts/<hostname>/configuration.nix` for each target host.

### 3. Check if already enabled

If the option is already present in the host's `etu = { }` block (even if set to false), inform the user and skip that host.

### 4. Insert the enable line

Find the right location within the `etu = { ... }` block. Module options are grouped by category prefix. Insert the new line:
- After the last existing line that shares the same category prefix (e.g., after all `graphical.*` lines if adding `graphical.foo`)
- If no lines with that category exist yet, insert after the category's parent enable line (e.g., after `graphical.enable = true;`)
- If none of those exist, insert in alphabetical order by category, before `theme`, `user` lines

The line to insert is:
```nix
    <module.option>.enable = true;
```

Use the Edit tool to make the insertion. Be precise — match enough surrounding context to place it correctly.

### 5. Verify

Re-read each modified file to confirm the line was inserted correctly and the `etu = { }` block still looks well-formed.

## Example

Enabling `graphical.audacity` on `desktop-elis`:

Before (relevant excerpt):
```nix
    graphical.enable = true;
    graphical.sway.enable = true;
    graphical.fdm-printing.enable = true;
```

After:
```nix
    graphical.enable = true;
    graphical.sway.enable = true;
    graphical.audacity.enable = true;
    graphical.fdm-printing.enable = true;
```

(Insert alphabetically among the `graphical.*` lines.)

## Notes

- Do NOT create a new module file. This skill only enables existing modules.
- Do NOT add `= false;` lines — only add when enabling.
- Preserve existing indentation (2-space within `etu = { }`, 4-space for the options themselves).
- If the user specifies a value other than `true` (e.g., a string or list), use that value instead.
