---
name: new-host
description: Use this skill when the user wants to add a new host (workstation, laptop, server, or VPS) to this NixOS config repository.
argument-hint: <hostname> <type>
allowed-tools: [Read, Write, Edit, Glob, Grep, Bash]
---

# Add a New NixOS Host

The user wants to scaffold a new host. Arguments: $ARGUMENTS

Parse the arguments:
- First argument: the hostname (e.g., `laptop-work-caroline`, `server-foo`, `vps07`)
- Second argument: host type — one of `workstation`, `laptop`, `server`, `vps`
  - `workstation` and `laptop` are treated identically for scaffolding purposes
  - If omitted, infer from the hostname prefix (`desktop-`/`laptop-` → workstation/laptop, `server-`/`vps` → server/vps)

If anything is unclear, ask the user before writing files.

---

## How Blueprint Discovers Hosts

Blueprint auto-discovers hosts from `hosts/*/configuration.nix`. No manual entry in `flake.nix` is needed for `nixosConfigurations`. Only remote-deployable servers need a `mkDeploy` entry in `flake.nix`.

---

## Files to Create

### `hosts/<hostname>/configuration.nix`

**Workstation / Laptop scaffold:**
```nix
{
  config,
  flake,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware.nix
    flake.nixosModules.default
  ];

  networking.hostName = "<hostname>";
  networking.hostId = "XXXXXXXX"; # placeholder — user must fill in

  etu = {
    stateVersion = "26.05";

    base.fish.enableUserZoxideCd = true;
    development.enable = true;
    graphical.enable = true;
    graphical.sway.enable = true;
    theme.enable = true;
    user.enable = true;

    base.sanoid.datasets = {
      "zroot/safe/data".use_template = [ "data" ];
      "zroot/safe/home".use_template = [ "home" ];
    };
  };
}
```

**Server / VPS scaffold:**
```nix
{
  config,
  flake,
  ...
}:
{
  imports = [
    ./hardware.nix
    flake.nixosModules.default
  ];

  networking.hostName = "<hostname>";
  networking.hostId = "XXXXXXXX"; # placeholder — user must fill in

  etu = {
    stateVersion = "26.05";

    user.enable = true;
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
}
```

### `hosts/<hostname>/hardware.nix`

**Workstation / Laptop scaffold:**
```nix
{
  config,
  lib,
  modulesPath,
  inputs,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./disko.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 15;

  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.kernelModules = [ ];

  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.autoScrub.enable = true;

  hardware.enableRedistributableFirmware = true;

  fileSystems.${config.etu.dataPrefix}.neededForBoot = true;
  fileSystems."${config.etu.dataPrefix}/home".neededForBoot = true;
  fileSystems.${config.etu.localPrefix}.neededForBoot = true;
  fileSystems."/nix".neededForBoot = true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
```

**Server / VPS scaffold** (minimal — user will likely replace with nixos-anywhere output):
```nix
{
  config,
  lib,
  modulesPath,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "sd_mod"
  ];
  boot.kernelModules = [ ];

  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.autoScrub.enable = true;

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [
      "defaults"
      "size=2G"
      "mode=755"
    ];
  };

  fileSystems."/nix" = {
    device = "zroot/local/nix";
    fsType = "zfs";
  };

  fileSystems.${config.etu.dataPrefix} = {
    device = "zroot/safe/data";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems.${config.etu.localPrefix} = {
    device = "zroot/local/data";
    fsType = "zfs";
    neededForBoot = true;
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
```

---

## Files to Edit

### `pubkeys.nix` — add the host system key

Read the file first. Add a new entry under `systems`:
```nix
    # <description of host>
    <hostname> = "ssh-ed25519 PLACEHOLDER_REPLACE_WITH_REAL_KEY";
```

Tell the user: the real key is at `/etc/ssh/ssh_host_ed25519_key.pub` on the new host. They must replace `PLACEHOLDER_REPLACE_WITH_REAL_KEY` once the system is installed.

### `secrets-registry.nix` — add the host key shorthand

Read the file. In the `h = { ... }` block, add:
```nix
    <hostname> = [ sys.<hostname> ];
```

Also add `sys.<hostname>` to `h.all` if this host should decrypt all shared secrets (typically true for servers and workstations). Do NOT add to `h.workstations` unless the user confirms it.

For the existing secrets that all hosts should access (`hashed-root-password` etc.), their `hostKeys` already use `h.all` — so adding the host to `h.all` covers them automatically.

### `Justfile` — add build (and deploy) recipes

Read the Justfile first. Add a build recipe in the `[group('build')]` section, keeping recipes in alphabetical order:
```justfile
# Build the <hostname> host
[group('build')]
build-<hostname>:
    nom build '.#nixosConfigurations.<hostname>.config.system.build.toplevel'
```

Add `build-<hostname>` to the `build-all` recipe's dependency list (keeping alphabetical order).

**For servers/VPS only**, also add a deploy recipe:
```justfile
# Deploy the <hostname> host
[group('deploy')]
deploy-<hostname>:
    deploy --skip-checks --targets '.<hostname>'
```

### `flake.nix` — add deploy node (servers/VPS only)

Read the file. In the `deploy.nodes = { ... }` block, add:
```nix
            <hostname> = mkDeploy { name = "<hostname>"; };
```

### `AGENTS.md` and `README.md` — update host tables

Add a row for the new host to the Hosts table in both files. Use the existing rows as a template. For the deployment method column:
- Workstation/laptop: `nixos-rebuild` locally
- Server/VPS: `deploy .<hostname>`

---

## After Writing Files

1. Remind the user of the TODOs they must complete manually:
   - Generate a real ZFS `hostId`: `head -c 8 /etc/machine-id | od -An -tx1 -v | tr -d ' '` on the new host
   - Replace the `PLACEHOLDER_REPLACE_WITH_REAL_KEY` in `pubkeys.nix` with the real host SSH key from `/etc/ssh/ssh_host_ed25519_key.pub`
   - Re-encrypt all secrets once the real key is in place: `agenix -r`
   - Customise `configuration.nix` with actual hardware modules, module enables, and any host-specific settings
   - Replace the scaffold `hardware.nix` with real hardware config (or the output of `nixos-generate-config`)

2. Run `just nix-fmt` to format the new files.
