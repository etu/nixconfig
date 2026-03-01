# AGENTS.md — Guide for AI Agents

> **Important:** Whenever you make changes to this repository that affect its
> structure, tooling, conventions, hosts, modules, or workflows, please update
> this file **and** the [README.md](./README.md) accordingly so that both
> remain accurate for future agents and human contributors.

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
| `data.nix` | Public, non-secret data: SSH public keys for all users and hosts; also exposes `ageModules` used by host configs |
| `secrets.nix` | Maps each `.age` secret file to the public keys that may decrypt it (used by agenix) |
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
| `data/` | Exposes `data.nix` values into NixOS option space |
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

Secrets are managed with [agenix](https://github.com/ryantm/agenix):

- **`secrets.nix`** — declares each `.age` file and the list of SSH public keys
  (users + hosts) that are allowed to decrypt it. Edit this file when adding
  new secrets or rekeying existing ones.
- **`data.nix`** — contains `pubkeys` with the public SSH keys for all users
  (`etu`, `concate`) and all host systems. Update this when adding a new host
  or rotating keys.
- **`secrets/`** — directory of encrypted `.age` files. Never commit plaintext
  secrets here.
- **`data.nix` → `ageModules`** — maps logical secret names to their `.age`
  file paths and (optionally) owner/path overrides. Used by NixOS host configs.

To re-encrypt secrets after adding a new key:

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
- **Public keys in `data.nix`**: all SSH public keys (user and host) live in
  `data.nix`. Update it when adding hosts or rotating keys.
- **blueprint conventions**: the flake uses blueprint, so follow its directory
  conventions for hosts, modules, and packages.
- **`just` for tasks**: prefer `just <recipe>` over raw `nix`/`deploy` commands
  to stay consistent with how the repo is managed.
- **Keep docs updated**: whenever you add, remove, or significantly change a
  host, module, workflow, or tool, update both `README.md` (human-facing
  overview) and `AGENTS.md` (agent-facing guide) to reflect the change.

---

## Adding a New Host

1. Create `hosts/<new-hostname>/` with at minimum a `default.nix` (or whatever
   blueprint expects).
2. Add the host's SSH public key to `data.nix` under `pubkeys.systems`.
3. Add the host to `secrets.nix` as needed (to grant it access to relevant
   secrets).
4. If the host should be remotely deployable, add a `mkDeploy` entry in
   `flake.nix` and a corresponding recipe in `Justfile`.
5. Update `README.md` and this `AGENTS.md` to document the new host.

---

## Adding a New Secret

1. Create the encrypted file: `agenix -e secrets/<path/to/secret>.age`
2. Add the entry to `secrets.nix` with the appropriate public keys.
3. Add a logical reference in `data.nix` under `ageModules` if the secret
   needs to be referenced by name in NixOS configs.
4. Run `agenix -r` to re-encrypt if you changed which keys have access.
