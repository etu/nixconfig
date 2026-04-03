---
name: add-secret
description: Use this skill when the user wants to add a new agenix secret to this NixOS config repository.
argument-hint: <secret-name> <dir> <host-or-group> [host-or-group ...]
allowed-tools: [Read, Write, Edit, Glob, Grep, Bash]
---

# Add a New Agenix Secret

The user wants to add a new secret managed by agenix. Arguments: $ARGUMENTS

Parse the arguments:
- First argument: the attribute name for the secret (e.g., `my-service-token`, `wireguard-private-key`)
- Second argument: the subdirectory under `secrets/` (e.g., `server-main-elis`, `workstations`, `any`)
- Remaining arguments: which hosts or groups can decrypt this secret. Each is either:
  - A host name: `desktop-elis`, `laptop-private-elis`, `laptop-work-elis`, `desktop-caroline`, `laptop-private-caroline`, `server-main-elis`, `server-sparv`, `vps06`
  - A convenience group: `all`, `workstations`

If arguments are missing or ambiguous, ask the user before proceeding.

## How Secrets Work in This Repo

`secrets-registry.nix` is the single source of truth. It defines all secrets with their file path and `hostKeys` (the SSH public keys allowed to decrypt them). From it, `secrets.nix` and `config.etu.data.ageModules` are derived automatically.

The `etu` user keys (desktop-elis, laptop-private-elis, laptop-work-elis) are **always** prepended to every secret's `hostKeys` — they're defined as `etu` in the registry's `let` block.

## Step 1 — Create the encrypted secret file

Tell the user to run this command interactively (it opens an editor):
```
agenix -e secrets/<dir>/<secret-name>.age
```

Instruct them to type `! agenix -e secrets/<dir>/<secret-name>.age` in the Claude Code prompt to run it in the session, then enter the secret value in the editor that opens.

Wait for the user to confirm the file was created before proceeding.

## Step 2 — Read secrets-registry.nix

Read `secrets-registry.nix` to understand the current structure and find the right section to insert the new entry.

Choose the insertion point:
- Find the section comment that matches the `<dir>` (e.g., `# server-main-elis secrets`)
- If no matching section exists, add a new section at the end before the closing `}`

## Step 3 — Add the registry entry

The standard entry shape is:
```nix
  <secret-name> = {
    file = ./secrets/<dir>/<secret-name>.age;
    hostKeys = etu ++ h.<host-or-group>;
  };
```

For multiple hosts: `hostKeys = etu ++ h.host1 ++ h.host2;`

Optional fields (only add if the user specified them):
- `owner = "<unix-user>";` — if the secret must be owned by a specific system user
- `group = "<unix-group>";`
- `mode = "0400";` — default is already 0400, only set if different
- `path = "/explicit/path";` — if the secret must land at a specific path (not the default `/run/agenix/`)
- `symlink = false;` — required when `path` points into a ZFS persist dataset

Use the Edit tool to insert the new block at the chosen location in `secrets-registry.nix`. Keep the existing section's style (blank line between entries, correct indentation of 2 spaces).

## Step 4 — Stage the .age file

Run:
```bash
git add secrets/<dir>/<secret-name>.age
```

## Step 5 — Show how to consume the secret

Explain to the user how to reference the secret from a NixOS module or host config:

**In a host's `configuration.nix`** (makes the secret available at runtime):
```nix
age.secrets = {
  inherit (config.etu.data.ageModules) <secret-name>;
};
```

**In a NixOS module or service config** (reference the decrypted path):
```nix
someOption = config.age.secrets.<secret-name>.path;
```

## Notes

- The `etu` variable in `secrets-registry.nix` already covers the three personal computer user keys — never add those as individual `h.<host>` entries.
- The `.age` file must exist on disk before `nix flake check` will pass.
- If the secret is for a system service with a specific owner (e.g., `syncoid`, `nextcloud`), remind the user to set `owner`.
- Secrets with a custom `path` pointing into `/data/...` (ZFS persist) need `symlink = false`.
