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
| `flake.nix` | Main flake definition: inputs, blueprint wiring, deploy-rs nodes, and the live-iso package alias |
| `flake.lock` | Locked versions of all flake inputs |
| `pubkeys.nix` | All SSH public keys for users and host systems — standalone, no imports |
| `secrets-registry.nix` | **Single source of truth** for all agenix secrets: file path, owner/path/symlink options, and the list of host keys allowed to decrypt each secret |
| `secrets.nix` | Auto-derived from `secrets-registry.nix` — do not edit directly; consumed by the agenix CLI |
| `secrets/` | Directory of agenix-encrypted secret files (`.age`) |
| `hosts/` | Per-machine NixOS configurations |
| `modules/` | Reusable NixOS (`modules/nixos/`) and home-manager (`modules/home/`) modules |
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
| `desktop-elis` | Desktop | `nixos-rebuild` locally | Primary desktop for etu |
| `desktop-caroline` | Desktop | `nixos-rebuild` locally | Desktop for Caroline |
| `laptop-private-elis` | Laptop | `nixos-rebuild` locally | Private laptop (T495); pushes ZFS snapshots to `server-main-elis` |
| `laptop-private-caroline` | Laptop | `nixos-rebuild` locally | Private laptop for Caroline |
| `laptop-work-elis` | Laptop | `nixos-rebuild` locally | Work laptop; pushes ZFS snapshots to `server-main-elis` |
| `server-main-elis` | Server | `deploy .#server-main-elis` | Home file server; also a Nix build machine, ZFS snapshot target, runs Home Assistant |
| `server-sparv` | Server | `deploy .#server-sparv` | On-location server for speliarvika.se, LAN cache |
| `vps06` | VPS | `deploy .#vps06` | Runs Gitea, ip.failar.nu, Matrix homeserver |
| `live-iso` | ISO | `nix build .#iso` | Live ISO of this config; not deployed remotely |

---

## Modules

### `modules/nixos/`

Reusable NixOS modules covering:

| Directory | Purpose |
|-----------|---------|
| `base/` | Base system settings shared across hosts |
| `data/` | Defines `config.etu.data` option: derives `ageModules` from `secrets-registry.nix` and imports `pubkeys.nix` |
| `development/` | Development tooling options |
| `games/` | Gaming-related configuration |
| `graphical/` | Graphical desktop (Sway/Wayland) stack |
| `services/` | Various service configurations (e.g. Nextcloud, Gitea, Home Assistant) |
| `theme/` | System-wide theming (Catppuccin) |
| `user/` | User account definitions |
| `work/` | Work-specific settings |
| `default.nix` | Module entry point, imports all sub-modules |

### `modules/home/`

Home-manager modules for user-level configuration:

| Directory | Purpose |
|-----------|---------|
| `alacritty/` | Alacritty terminal emulator config |
| `emacs/` | Emacs configuration |
| `firefox/` | Firefox browser settings |
| `fish/` | Fish shell configuration |
| `flatpak-overrides/` | Flatpak permission overrides |
| `foot/` | Foot terminal emulator config |
| `graphical-dotfiles/` | Miscellaneous graphical dotfiles |
| `htop/` | htop config |
| `kanshi/` | kanshi (display management) config |
| `mako/` | mako notification daemon config |
| `sway/` | Sway compositor config |
| `tmux/` | tmux config |
| `vscode/` | VS Code settings and extensions |
| `waybar/` | Waybar (status bar) config |

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
just build                        # Build current host (auto-detected via `hostname`)
just build-<hostname>             # Build a specific host
just build-all                    # Build all hosts (plus the live ISO)
just build-live-iso               # Build the live ISO image
```

### Deploy

```sh
just deploy-server-main-elis
just deploy-server-sparv
just deploy-vps06
```

These call `deploy --skip-checks --targets '.#<hostname>'` using deploy-rs.

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
| `secrets.nix` | Derived from registry; maps `.age` file paths → `publicKeys` for the agenix CLI | **No** — auto-generated |
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
- `secrets.nix` maps each entry's `file` path to `{ publicKeys = hostKeys; }`
  for the agenix CLI.

`secrets.nix` is the only derived file — do not edit it by hand.

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
use `nix-installer-action`. Remove it again once the upstream issue is resolved.

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
| `nixpkgs` | Main nixpkgs (nixos-unstable) |
| `nixpkgs-22-11` | Legacy nixpkgs pinned to 22.11 |
| `nixos-hardware` | Hardware-specific NixOS modules |
| `agenix` | Encrypted secret management |
| `blueprint` | Flake layout / convention helper |
| `catppuccin` | Catppuccin theme for NixOS/home-manager |
| `deploy-rs` | Remote NixOS deployment |
| `disko` | Declarative disk partitioning |
| `emacs-overlay` | Up-to-date Emacs packages |
| `home-manager` | User environment management |
| `impermanence` | Opt-in persistence on ephemeral root |
| `ip-failar-nu` | Custom service: ip.failar.nu |
| `nur` | Nix User Repository |

All inputs follow `nixpkgs` via `.follows` where possible to avoid duplicate
versions.

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

- **Removed option defaults**: modules sometimes drop their default values,
  making previously-optional config mandatory.
  - Example (26.05): `services.grafana.settings.security.secret_key` lost its
    default. Fix: create an agenix secret with the existing key (retrieve from
    the running host if upgrading), add an entry to `secrets-registry.nix`, then
    set the option via `"$__file{${config.age.secrets.<name>.path}}"`.

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

`secrets.nix` is derived automatically — do not edit it.

---

## Checklist Before Finishing a Task

Before completing any task, check whether your changes require updating this
file or `README.md`. Update them if you did any of the following:

- [ ] Added, removed, or renamed a **host**
- [ ] Added or changed a **Justfile recipe**
- [ ] Added or changed a **secret** (new `.age` file, `secrets-registry.nix`, `pubkeys.nix`)
- [ ] Added or changed a **module** (`modules/nixos/` or `modules/home/`)
- [ ] Changed a **CI workflow** (`.github/workflows/`)
- [ ] Changed **tooling or conventions** (formatters, linters, deploy method)
- [ ] Discovered a new **upgrade breakage pattern** worth documenting
