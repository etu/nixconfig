# AGENTS.md — Guide for AI Agents

> **Important:** Whenever you make changes to this repository, update this file
> **and** [README.md](./README.md) before finishing. This applies to: adding or
> removing hosts, adding Justfile recipes, adding/changing secrets, changing
> modules or CI workflows, and discovering new upgrade breakage patterns.
> A reminder checklist is at the **bottom of this file**.

---

## Repository Overview

This is a personal NixOS configuration repository for **etu** (Elis Hirwing).
It manages multiple machines using [Nix Flakes](https://nixos.wiki/wiki/Flakes),
[home-manager](https://github.com/nix-community/home-manager),
[deploy-rs](https://github.com/serokell/deploy-rs) for remote deployments, and
[agenix](https://github.com/ryantm/agenix) for secret management.

The flake is structured using [blueprint](https://github.com/numtide/blueprint)
which provides a conventional layout that is recursively merged with custom
flake outputs.

---

## Top-Level Files and Directories

| Path | Description |
|------|-------------|
| `flake.nix` | Main flake definition: inputs, blueprint wiring (with an explicit `systems` list, since nixpkgs 26.11 dropped `x86_64-darwin` from blueprint's defaults), deploy-rs nodes, and the live-iso package alias |
| `flake.lock` | Locked versions of all flake inputs |
| `pubkeys.nix` | All SSH public keys for users and host systems — standalone, no imports; also the Nix remote builder keys (`builders` for the Mac's builder client key and `builderHostKeys` for the base64 host keys) |
| `secrets-registry.nix` | **Single source of truth** for all agenix secrets: file path, owner/path/symlink options, and the list of host keys allowed to decrypt each secret |
| `agenix-rules.nix` | Auto-derived from `secrets-registry.nix` — do not edit directly; consumed by the agenix CLI |
| `secrets/` | Directory of agenix-encrypted secret files (`.age`) |
| `hosts/` | Per-machine NixOS configurations |
| `modules/` | Reusable NixOS (`modules/nixos/`), nix-darwin (`modules/darwin/`) and home-manager (`modules/home/`) modules |
| `packages/` | Custom Nix packages defined in this repo |
| `devshell.nix` | `nix develop` shell with all tools needed for day-to-day work |
| `Justfile` | Task runner (via [just](https://just.systems/)) wrapping common nix, build, deploy, format, and update commands |
| `.envrc` | [direnv](https://direnv.net/) integration — runs `use flake` automatically |
| `.statix.toml` | Configuration for [statix](https://github.com/nerdypepper/statix) (disables `repeated_keys` check) |
| `.yamllint` | YAML lint configuration |
| `.github/workflows/` | CI/CD GitHub Actions workflows |

---

## Hosts

Each host lives in `hosts/<hostname>/` and is automatically picked up by blueprint.

| Host | Type | Deployment method | Notes |
|------|------|-------------------|-------|
| `macbook-air-elis` | Laptop (macOS) | `just activate` locally | MacBook Air M5, `aarch64-darwin`, managed with nix-darwin via `darwin-configuration.nix`; built in CI on a `macos-latest` runner; builds the x86_64-linux hosts on `server-main-elis` (see *Building and deploying from macOS*) |
| `server-main-elis` | Server | `deploy .#server-main-elis` | Home file server; also the Nix remote builder for the MacBook (builds the x86_64-linux hosts from macOS), ZFS snapshot target, runs Home Assistant |
| `server-sparv` | Server | `deploy .#server-sparv` | On-location server for speliarvika.se, LAN cache, game servers (Valheim, Project Zomboid, Minecraft) |
| `vps06` | VPS | `deploy .#vps06` | Runs Gitea, ip.failar.nu, Matrix homeserver |
| `live-iso` | ISO | `nix build .#iso` | Console-only live ISO of this config (no graphical environment); not deployed remotely |

---

## Modules

### `modules/nixos/`

Reusable NixOS modules covering:

| Directory | Purpose |
|-----------|---------|
| `base/` | Base system settings shared across hosts, including the blacklist for the compromised Comodo AAA Services certificate, and `boot.zfs.forceImportRoot = false` (the 26.11 default; after importing a host's pool from another system, such as a rescue ISO, `zpool import -f` it by hand once) |
| `data/` | Defines `config.etu.data` option: derives `ageModules` from `secrets-registry.nix` and imports `pubkeys.nix` |
| `development/` | Development tooling; only `git/` is left (`etu.development.git.enable`, used by `server-main-elis` and `live-iso`), which doesn't sign commits by default |
| `graphical/` | Only `theme/`, declaring the `etu.graphical.theme.fonts` options read by `homeModules.emacs` (mirrored by the darwin module). The NixOS desktop (sway, dms-shell, catppuccin theme, desktop development modules) was removed; the desktop is the nix-darwin MacBook |
| `hacks/` | **Temporary overlay hacks** — patches and package overrides waiting on upstream fixes. Check here during upgrades to see if anything can be removed. |
| `services/` | Various service configurations (e.g. Nextcloud, Gitea, Home Assistant) |
| `user/` | User account definitions |
| `default.nix` | Module entry point, imports all sub-modules |

### `modules/darwin/`

nix-darwin modules, exposed by blueprint as `darwinModules.<name>` and
imported by darwin hosts through `flake.darwinModules.default`. They mirror
`modules/nixos/` and reuse its `etu.*` option names, so shared home modules
that read `osConfig.etu.*` work unchanged on macOS.

| Directory | Purpose |
|-----------|---------|
| `base/` | Base settings; enables the base modules below by default, blacklists the compromised Comodo AAA Services certificate, and defines `etu.dataPrefix` (empty by default: macOS has no ephemeral root or `/data`, so paths like the Emacs org agenda's `<dataPrefix>$HOME/.dotfiles` resolve to the home directory) |
| `base/emacs/` | `etu.base.emacs.*`: emacs-overlay, icon font, unfree language servers, imports `homeModules.emacs`. Host-specific Emacs Lisp (like the work MacBook's `~/tvnu/.lisp` loader) goes in `etu.base.emacs.extraConfig`, which is inserted into the shared config |
| `base/fish/` | `etu.base.fish.*`: fish system wide and in `/etc/shells`, imports `homeModules.fish` and adds macOS-only abbreviations (`l` for `ls -alh`) |
| `base/htop/` | `etu.base.htop.enable`: imports `homeModules.htop` |
| `base/nix/` | `etu.base.nix.*`: flakes, trusted users, binary caches, `allowUnfree` list, `remoteBuilds` (remote build on `server-main-elis` via `nix.distributedBuilds` + `nix.buildMachines`; off by default, enabled on `macbook-air-elis`, see *Building and deploying from macOS*), weekly garbage collection (older than 7 days) and daily store optimisation |
| `base/packages/` | Command line tools (mirrors `modules/nixos/base/packages`, minus Linux-only ones, plus `watch`, which macOS lacks, and the GitHub CLI `gh`) |
| `base/spell/` | `etu.base.spell.enable`: aspell and hunspell with English and Swedish dictionaries for the user (Emacs enables flyspell in text buffers), like the NixOS module |
| `base/ssh/` | `etu.base.ssh.enable`: ssh client using the macOS ssh-agent with key passphrases in the Keychain (`UseKeychain`, `AddKeysToAgent`), and a login agent loading the keys into the ssh-agent |
| `base/system/` | macOS defaults: the flake's git revision as `system.configurationRevision` (for `darwin-version`), dark mode, auto-hiding Dock without recent apps, battery percentage and Bluetooth in the menu bar, Caps Lock → Control, faster key repeat, Touch ID for `sudo` (with `pam_reattach`, so it also works inside tmux) |
| `base/tailscale/` | `etu.base.tailscale.enable`: `tailscaled` as a launchd daemon |
| `base/tmux/` | `etu.base.tmux.enable`: imports `homeModules.tmux`, always starts fish (tmux otherwise uses `$SHELL`, which is zsh on macOS), and overrides the status bar's load average (the shared module reads Linux-only `/proc/loadavg`) with `sysctl -n vm.loadavg` |
| `development/claude-code/` | `etu.development.claude-code.enable`: Claude Code and claude-monitor for the user |
| `development/direnv/` | `etu.development.direnv.enable`: direnv with nix-direnv for the user; the fish hook comes from `homeModules.fish` |
| `development/git/` | `etu.development.git.enable`: git system wide, imports `homeModules.git` and always signs commits (SSH key through the ssh-agent) |
| `development/vscode/` | `etu.development.vscode.enable` / `enableWork`: imports `homeModules.vscode` and allows its unfree packages. `enableWork` adds the work extensions and settings; nothing here sets it, but it is kept for use by an external work flake |
| `graphical/fdm-printing/` | `etu.graphical.fdm-printing.enable`: OpenSCAD for the user (FreeCAD isn't available for macOS); also read by `homeModules.vscode` for the OpenSCAD extension |
| `graphical/aerospace/` | `etu.graphical.aerospace.enable`: AeroSpace tiling window manager via `services.aerospace`, with bindings mirroring the former NixOS sway setup (Alt instead of Super, configurable with `etu.graphical.aerospace.modifier`, Emacs-style `b`/`f`/`p`/`n` directions, Dvorak key mapping, workspaces I–X on keys 1–0, resize mode) |
| `graphical/ghostty/` | `etu.graphical.ghostty.enable`: imports `homeModules.ghostty` for the user and adds the macOS specifics (`ghostty-bin`, always start fish, quit after the last window closes) |
| `graphical/theme/` | `etu.graphical.theme.*`: font options (macOS defaults: `JetBrains Mono`, 12) and the font package |
| `user/` | `etu.user.username`: `system.primaryUser` and the user's home directory; also `etu.user.{realname,email,signingKey}` (same defaults as NixOS) for `homeModules.git`. `etu.user.restow.enable` imports `homeModules.restow`. Sets up home-manager for the user (`useGlobalPkgs`, `useUserPackages`, `perSystem`, zsh) with `home.stateVersion` from `etu.stateVersion` |
| `default.nix` | Module entry point, imports all sub-modules, home-manager's darwin module (from the `home-manager-darwin` input) and NUR's darwin module |

### `modules/home/`

Home-manager modules for user-level configuration:

| Directory | Purpose |
|-----------|---------|
| `emacs/` | Emacs configuration (`emacs-pgtk` on Linux, native `emacs` on macOS; on macOS the Nix profiles are added to the wrapper's PATH, since apps started from Spotlight/launchd only get `/usr/bin:/bin:/usr/sbin:/sbin`, and `Emacs.app` is replaced by a launcher that starts the wrapped `bin/emacs` as a separate process, because a bundle that execs into another bundle's binary isn't managed by window managers like AeroSpace) |
| `fish/` | Fish shell configuration |
| `ghostty/` | Ghostty settings shared by NixOS and nix-darwin: font from `etu.graphical.theme.fonts`, block cursor, copy-on-select, and ssh shell integration (`ssh-terminfo`, `ssh-env`). Only the MacBook uses it now; `modules/darwin/graphical/ghostty` sets `ghostty-bin`, always starts fish and quits after the last window closes |
| `git/` | Git settings shared by NixOS and nix-darwin: identity and signing key from `etu.user.*`, SSH commit signing setup with an allowed signers file from `pubkeys.nix`, `git-*` helper scripts (from `bin/`, installed into the profile so they're in PATH on NixOS and macOS) and global ignores (including macOS filesystem artifacts on Darwin only). Hosts that need another identity (like the work MacBook) set `etu.user.email`. Whether to sign by default is set by the OS modules (`etu.graphical.gnupg.enable` on NixOS, always on darwin) |
| `htop/` | htop config |
| `restow/` | The `restow` command (installed into the profile): `git pull` in `~/.dotfiles` and `stow --dotfiles skeleton`. macOS only, imported by the darwin user module on hosts that set `etu.user.restow.enable` (the MacBook) |
| `tmux/` | tmux config |
| `vscode/` | VS Code settings and extensions |

---

## Development Environment

Enter the dev shell with:

```sh
nix develop
# or, with direnv configured:
direnv allow
```

The shell (defined in `devshell.nix`) provides:

- `just` — task runner
- `nom` (`nix-output-monitor`) — prettier Nix build output
- `deadnix` — detect unused Nix code
- `statix` — Nix linting / anti-pattern detection
- `yamllint` — YAML formatting checks
- `just-formatter` — Justfile formatter
- `agenix` — manage encrypted secrets
- `deploy-rs` — remote deployment tool
- `curl`, `jq`, `cacert` — utilities for update scripts

---

## Common Tasks (Justfile)

Run `just` to list all available recipes. Key groups:

### Build

```sh
just build                        # Build current host (auto-detected via `hostname`; darwinConfigurations on macOS, nixosConfigurations elsewhere)
just build-<hostname>             # Build a specific host
just build-all                    # Build all NixOS hosts (plus the live ISO)
just build-macbook-air-elis       # Build the nix-darwin MacBook config (macOS only)
just build-live-iso               # Build the live ISO image
```

### Deploy

```sh
just deploy-server-main-elis
just deploy-server-sparv
just deploy-vps06
```

These call `deploy --skip-checks --targets '.#<hostname>'` using deploy-rs.
From macOS, deploy-rs builds the x86_64-linux closure through the remote
builder (see *Building and deploying from macOS*) and copies it to the target.

`just activate` builds and activates the current host locally, picked by
`hostname`: `sudo darwin-rebuild switch` on macOS and
`sudo nixos-rebuild switch` elsewhere.

### Format / Lint

```sh
just nix-fmt                      # Format Nix files (nix fmt)
just yaml-fmt                     # Lint YAML files (yamllint)
just nix-fmt-check                # CI-style Nix format check (nix fmt -- --ci)
just yaml-fmt-check               # CI-style YAML check
just deadnix-fmt-check            # Check for unused Nix bindings
just statix-fmt-check             # Statix lint check
just all-fmt-check                # Run all format checks
```

### Update

```sh
just update-flake                 # Update all flake inputs (nix flake update)
just update-hass                  # Update Home Assistant container image tag
just update-zwavejs2mqtt          # Update zwavejs2mqtt container image tag
just update-mosquitto             # Update Mosquitto container image tag
just update-vscode-extensions     # Regenerate VS Code extension Nix expressions
just update-all                   # Run all updaters
```

### Nix Flake Utilities

```sh
just flake-show                   # nix flake show
just flake-check                  # nix flake check
```

---

## Building and deploying from macOS

The MacBook has no local x86_64-linux builder, so Nix builds the NixOS hosts
on `server-main-elis` (ssh-ng, as root, 8 jobs). The wiring lives in
`modules/darwin/base/nix/` behind `etu.base.nix.remoteBuilds`, which is **off
by default** (a builder needs the dedicated key on disk) and is enabled per
host — currently only in `hosts/macbook-air-elis/darwin-configuration.nix`:

- `nix.distributedBuilds = true` plus `nix.buildMachines = [ … ]`. Nix has to
  keep its default `builders = @/etc/nix/machines`: nix-darwin writes that
  file from `nix.buildMachines` only while distributed builds are on, and
  *clears* `builders` when they are off, which would silently disable remote
  builds.
- `nix.settings.builders-use-substitutes = true`, so the builder fetches
  substitutes instead of only receiving inputs copied from the Mac.
- Do **not** set `extra-platforms = x86_64-linux`: remote builders are picked
  up without it, while that setting makes Nix attempt foreign builds locally,
  which fail with "Undefined error: 0".

A builder needs:

- A dedicated **passphrase-less** key on the Mac (`~/.ssh/nix-builder`) —
  Nix's SSH can neither prompt for a passphrase nor use the ssh-agent. Its
  public key lives in `pubkeys.nix` under `builders` and is authorized for
  root on the builder through `etu.user.extraRootAuthorizedKeys` in the
  builder's host config (for `server-main-elis`).
- Its host key, base64-encoded, in `pubkeys.nix` under `builderHostKeys`
  (`ssh <host> "base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub"`). It is passed
  as the last field of `/etc/nix/machines`; without it Nix falls back to
  SSH's known_hosts files, which are not managed here.

Verify with `just build-vps06` (or any other x86_64-linux host): the builds
must run on `server-main-elis` instead of failing locally. Builder changes
reach the Mac with `just activate` and the authorized_keys side with a deploy
of `server-main-elis`.

---

## Formatting Requirements

All Nix code must pass:

1. **`nix fmt`** — uses the formatter configured in the flake (treefmt/nixfmt)
2. **`deadnix`** — no unused let-bindings or function arguments in `hosts/`, `modules/`, `packages/`
3. **`statix`** — no anti-patterns (except `repeated_keys`, which is explicitly disabled in `.statix.toml`)
4. **`yamllint`** — all YAML files must pass strict lint

Run `just all-fmt-check` to verify all checks locally before pushing.

---

## Secrets Management

Secrets are managed with [agenix](https://github.com/ryantm/agenix).

### File layout

| File | Purpose | Edit? |
|------|---------|-------|
| `pubkeys.nix` | All SSH public keys (users + host systems) | Yes — when adding hosts or rotating keys |
| `secrets-registry.nix` | **Single source of truth**: one entry per secret with `file`, optional agenix module fields (`owner`, `path`, `symlink`), and `hostKeys` | Yes — when adding/changing secrets |
| `agenix-rules.nix` | Derived from registry; maps `.age` file paths → `publicKeys` for the agenix CLI | **No** — auto-generated |
| `secrets/` | Encrypted `.age` files. Never commit plaintext here | Encrypted only |

### How it works

`secrets-registry.nix` is the only file you touch when managing secrets. It
imports `pubkeys.nix` and defines every secret as an attribute:

```nix
my-secret = {
  file     = ./secrets/server-foo/my-secret.age;  # path to .age file
  owner    = "someuser";                           # optional agenix options
  hostKeys = etu ++ h.server-foo;                  # keys that can decrypt
};
```

- `modules/nixos/data/default.nix` calls `builtins.removeAttrs secret ["hostKeys"]` on each
  registry entry to produce the `ageModules` attrset consumed by host/module configs via
  `config.etu.data.ageModules.<name>`.
- `agenix-rules.nix` maps each entry's `file` path to `{ publicKeys = hostKeys; }`
  for the agenix CLI.

`agenix-rules.nix` is the only derived file — do not edit it by hand.
Nextcloud administrator credentials are not managed by this repository.

### Re-encrypting after adding a key

```sh
agenix -r -i <identity-file>
```

---

## CI/CD Workflows

Located in `.github/workflows/`:

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `format.yml` | push / PR | Checks Nix fmt, YAML lint, deadnix, statix, and flake integrity |
| `pull_request.yaml` | PR → main | Builds dev shell, runs flake check, builds all hosts (no push to cache) |
| `cache.yml` | push to `main`/`cache` (`.nix`, `.age`, `flake.lock`) | Runs flake check and caches all host builds to Cachix |
| `deploy.yml` | push to `main` (`.nix`, `.age`, `flake.lock`) | Runs flake check then deploys remotely via deploy-rs |
| `update.yml` | scheduled | Automated flake/container update PRs |

Helper workflow files (`helper_*.yaml`) are reusable workflows called by the
above.

`helper_host_matrix.yaml` builds the host matrix from `nixosConfigurations`
and `darwinConfigurations`. Each entry carries the `runner`, nix `system` and
flake `attr` to build: NixOS hosts run on `ubuntu-24.04` (`x86_64-linux`),
nix-darwin hosts on `macos-latest` (`aarch64-darwin`).
`helper_cache_hosts.yaml` uses those, so the pull request, cache and update
workflows all build the MacBook too. Steps that only work on Linux (such as
`wimpysworld/nothing-but-nix`) are guarded with `if: runner.os == 'Linux'`.

### Pinning the Nix version in CI

If a Determinate Nix release breaks CI (e.g. `nix flake check` fails on GitHub
Actions but works locally), you can pin the Nix version installed by
`nix-installer-action` by adding a `with: nix-package-url:` argument pointing
to a specific release from `releases.nixos.org`. Example:

```yaml
- uses: DeterminateSystems/nix-installer-action@v21
  with:
    nix-package-url: https://releases.nixos.org/nix/nix-2.31.3/nix-2.31.3-x86_64-linux.tar.xz
```

Run `nix eval nixpkgs#nix.version --raw` locally to find the version your
nixpkgs provides. Apply the `with:` block to all seven workflow files that
use `nix-installer-action`. In `helper_cache_hosts.yaml` the tarball name uses
`${{ matrix.system }}` instead of `x86_64-linux`, since it also runs on macOS. Remove it again once the upstream issue is resolved.

### Required GitHub Secrets

| Secret | Used by |
|--------|---------|
| `CACHIX_AUTH_TOKEN` | Pushing build results to Cachix |
| `SSH_PRIVATE_KEY` | deploy-rs SSH access to remote hosts |
| `SSH_KNOWN_HOSTS` | Known-hosts verification for deploy-rs |
| `TS_OAUTH_CLIENT_ID` | Tailscale OAuth (deployment networking) |
| `TS_OAUTH_SECRET` | Tailscale OAuth secret |

---

## Key Dependencies (Flake Inputs)

| Input | Purpose |
|-------|---------|
| `nixpkgs` | nixpkgs for the NixOS hosts (`nixos-unstable-small`) |
| `nixpkgs-darwin` | nixpkgs for the nix-darwin hosts (`nixpkgs-unstable`, which waits for the darwin builds) |
| `nixos-hardware` | Hardware-specific NixOS modules |
| `agenix` | Encrypted secret management |
| `blueprint` | Flake layout / convention helper |
| `deploy-rs` | Remote NixOS deployment |
| `disko` | Declarative disk partitioning |
| `emacs-overlay` | Up-to-date Emacs packages |
| `home-manager` | User environment management (NixOS hosts) |
| `home-manager-darwin` | home-manager for the nix-darwin hosts, following `nixpkgs-darwin` |
| `impermanence` | Opt-in persistence on ephemeral root |
| `ip-failar-nu` | Custom service: ip.failar.nu |
| `nix-darwin` | macOS system configuration for `macbook-air-elis`, following `nixpkgs-darwin` |
| `nur` | Nix User Repository |

nixpkgs is split by platform. Blueprint builds the NixOS hosts with
`inputs.nixpkgs.lib.nixosSystem`, so they always use `nixpkgs`. The darwin
hosts get their packages from nix-darwin's own `nixpkgs` input, which follows
`nixpkgs-darwin`, and `modules/darwin` imports home-manager from
`home-manager-darwin`. Blueprint's per-system outputs (`packages/`,
`perSystem.*`) are built from `nixpkgs` on every platform, including darwin;
that only affects the small VS Code extension packages the MacBook uses.

All other inputs follow `nixpkgs` via `.follows` where possible to avoid
duplicate versions.

---

## Conventions

- **Nix formatting**: use `nix fmt` (treefmt-based) — never manually reformat
  unless the formatter is unavailable.
- **No unused code**: `deadnix` will fail CI if unused let-bindings or function
  arguments are left in `hosts/`, `modules/`, or `packages/`.
- **No anti-patterns**: `statix` enforces Nix idioms. The only disabled rule is
  `repeated_keys` (see `.statix.toml`).
- **Secrets stay encrypted**: never commit decrypted secret content. Only `.age`
  files go into `secrets/`.
- **Public keys in `pubkeys.nix`**: all SSH public keys (user and host) live in
  `pubkeys.nix`. Update it when adding hosts or rotating keys.
- The `laptop-work-elis` SSH key is retained for a separately managed device;
  this repository no longer configures that host.
- The `macbook-air-elis` key (user key on the nix-darwin MacBook) is part of
  `etu.computers` and the `etu` key set in `secrets-registry.nix`.
- **nix-darwin hosts**: blueprint loads `hosts/<name>/darwin-configuration.nix`
  into `darwinConfigurations.<name>`. They import `flake.darwinModules.default`
  (`modules/darwin/`) instead of the NixOS-only `modules/nixos/`, and set
  `etu.*` options just like NixOS hosts do. To bring a NixOS feature to
  macOS, add a matching module under `modules/darwin/` using the same `etu.*`
  option names, and have it import shared home modules into
  `home-manager.users.${config.etu.user.username}`. Shared home modules that
  read `osConfig.etu.*` then work unchanged, as long as the darwin modules
  declare every option they read. Darwin hosts have no `users/` directory:
  `modules/darwin` imports home-manager itself (like `modules/nixos`) and
  sets it up for `etu.user.username`, so host-specific settings go in
  `darwin-configuration.nix` as `etu.*` options (or
  `home-manager.users.<user>` there if no option fits). The emacs
  home module picks `pkgs.emacs` on darwin and `pkgs.emacs-pgtk` on Linux.
  Unfree packages go in `etu.base.nix.allowUnfree`, as on NixOS (it also
  covers home-manager via `useGlobalPkgs`). The NixOS hosts evaluate on macOS without
  import-from-derivation (keep it that way: catppuccin's IFD used to make
  even evaluation need a Linux builder), but building them needs an
  `x86_64-linux` builder — the remote builder on `server-main-elis`
  covers that (see *Building and deploying from macOS*).
- **Building from macOS**: the x86_64-linux hosts are built on
  `server-main-elis` from the MacBook via `nix.buildMachines` — read
  *Building and deploying from macOS* before changing `nix.*` settings there.
- **Mittens root access**: the `mittens` key in `pubkeys.nix` is authorized for
  root SSH access on `server-sparv`.
- **blueprint conventions**: the flake uses blueprint, so follow its directory
  conventions for hosts, modules, and packages.
- **`just` for tasks**: prefer `just <recipe>` over raw `nix`/`deploy` commands
  to stay consistent with how the repo is managed.
- **Keep docs updated**: whenever you add, remove, or significantly change a
  host, module, workflow, or tool, update both `README.md` (human-facing
  overview) and `AGENTS.md` (agent-facing guide) to reflect the change.

---

## Performing System Upgrades

The upgrade workflow mirrors what `.github/workflows/update.yml` automates.
When doing a manual upgrade, follow these steps in order:

1. **Update flake inputs**: `just update-flake` (runs `nix flake update`)
2. **Run all updaters**: `just update-all` (updates HASS container tag, VSCode
   extensions, etc.)
3. **Lint/format check**: `just all-fmt-check` — must be clean before
   continuing.
4. **Flake check**: `just flake-check` — fix any evaluation errors first.
5. **Build all hosts**: `just build-<hostname>` for each host, or let CI do it.
6. **Commit**, then **deploy** each remotely-managed server and confirm it is
   healthy before pushing. Deployment order: `server-main-elis` →
   `server-sparv` → `vps06`.
7. **Push** once all deployments are confirmed.

### Common nixpkgs upgrade breakages to watch for

These patterns have caused evaluation or build failures in past upgrades:

- **Renamed options**: nixpkgs occasionally renames or restructures NixOS
  options between releases. If `nix flake check` or a host build fails with
  "option does not exist", search nixpkgs for the new option name.
  - Example (26.05): `services.homepage-dashboard.environmentFile` (scalar)
    was renamed to `environmentFiles` (list).

- **Removed NixOS options read by flake input modules**: when nixpkgs
  removes an option, a module from another input that still *reads* it
  breaks evaluation too, even if this repo never sets it. Update that input
  (`nix flake update <input>`) once upstream has adapted.
  - Example (26.11): the scripted initrd was removed, taking
    `boot.initrd.systemd.enable` with it. impermanence still read it
    ("can no longer be used since it's been removed") until its
    2026-10-23 release. All hosts already used systemd in stage 1, so
    boot behaviour didn't change.

- **Blacklisted CA certificates removed upstream**: the CA bundle build
  fails with "Certs in blocklist but not in cert store" when an entry in
  `security.pki.caCertificateBlacklist` (set in both `base` modules) is no
  longer shipped by nss. Remove that entry from both modules. Since every
  system depends on the bundle, all hosts fail to build at once.
  - Example (26.11): nss 3.131.1 dropped `emSign ECC Root CA - G3`.

- **Removed option defaults**: modules sometimes drop their default values,
  making previously-optional config mandatory.
  - Example (26.05): `services.grafana.settings.security.secret_key` lost its
    default. Fix: create an agenix secret with the existing key (retrieve from
    the running host if upgrading), add an entry to `secrets-registry.nix`, then
    set the option via `"$__file{${config.age.secrets.<name>.path}}"`.

- **Upstream service hardening**: nixpkgs keeps adding systemd hardening to
  service modules, which can conflict with the shared `perms` hardening set in
  `hosts/server-main-elis/services/usenet.nix` ("has conflicting definition
  values"). `perms` is therefore all `lib.mkDefault` (via
  `lib.mapAttrs (_: lib.mkDefault)`): upstream values win where a module sets
  them, and `perms` fills the gaps (nzbget and lidarr set no hardening at
  all). Where upstream's value is weaker than ours, override it with
  `lib.mkForce` in that service's block.
  - Example (26.11): bazarr gained `PrivateTmp = "disconnected"` (stricter than
    our `true`, so upstream's applies) and `ProtectSystem = "full"` (weaker
    than our `"strict"`, so the bazarr block sets `lib.mkForce "strict"` to
    keep its `ReadWritePaths` meaningful).

- **New agenix secrets**: when adding a new `.age` file, always `git add` it
  before running `nix flake check` — the file must be tracked for Nix to find
  it through the flake's git source.

- **Nextcloud major versions**: nixpkgs only allows single-step major upgrades
  (e.g. 32 → 33). After an update, an evaluation warning will appear if the
  running version is behind the latest. Handle this as a *separate* step after
  the main upgrade is confirmed stable.

- **deploy-rs rollback**: if a deployment activates but a systemd unit fails,
  deploy-rs will automatically roll back. On the next attempt the issue may be
  transient (e.g. a new upstream service failing on first start). Always check
  `journalctl -u <failing-unit>` on the host to understand the failure before
  retrying.

- **Hacks module**: after upgrading, review `modules/nixos/hacks/default.nix`.
  Each entry has a `# Track:` URL pointing to the upstream PR or issue. If the
  fix has landed in nixpkgs, remove the overlay and its patch file.

- **VSCode extension updates**: `just update-vscode-extensions` (part of
  `just update-all`) updates all extension version pins. If you want to hold
  back a specific extension (e.g. for manual review), simply don't stage that
  file when committing.

- **CI Nix version drift**: a Determinate Nix release can break CI even when
  local builds succeed. If this happens, pin the Nix version in all seven
  workflow files by adding `with: nix-package-url:` to the
  `nix-installer-action` step — see the *Pinning the Nix version in CI*
  section under CI/CD Workflows for the exact snippet.

---

## Adding a New Host

1. Create `hosts/<new-hostname>/` with at minimum a `default.nix` (or whatever
   blueprint expects).
2. Add the host's SSH public key to `pubkeys.nix` under `systems`.
3. Add `h.<hostname>` shorthands in `secrets-registry.nix` and include them in
   the `hostKeys` of each secret that the new host should decrypt.
4. If the host should be remotely deployable, add a `mkDeploy` entry in
   `flake.nix` and a corresponding recipe in `Justfile`.
5. Update `README.md` and this `AGENTS.md` to document the new host.

---

## Adding a New Secret

1. Create the encrypted file: `agenix -e secrets/<path/to/secret>.age`
2. `git add` the new `.age` file (required before `nix flake check` can find it).
3. Add one entry to **`secrets-registry.nix`** with `file`, any agenix module
   options (`owner`, `path`, `symlink`), and `hostKeys`.
4. Reference the secret in the consuming module/host via
   `age.secrets.<name> = config.etu.data.ageModules.<name>;` and
   `config.age.secrets.<name>.path`.
5. Run `agenix -r` if you changed which keys have access.

`agenix-rules.nix` is derived automatically — do not edit it.

---

## Checklist Before Finishing a Task

Before completing any task, check whether your changes require updating this
file or `README.md`. Update them if you did any of the following:

- [ ] Added, removed, or renamed a **host**
- [ ] Added or changed a **Justfile recipe**
- [ ] Added or changed a **secret** (new `.age` file, `secrets-registry.nix`, `pubkeys.nix`)
- [ ] Added or changed a **module** (`modules/nixos/`, `modules/darwin/` or `modules/home/`)
- [ ] Changed a **CI workflow** (`.github/workflows/`)
- [ ] Changed **tooling or conventions** (formatters, linters, deploy method)
- [ ] Discovered a new **upgrade breakage pattern** worth documenting
