[![Cache 📝](https://github.com/etu/nixconfig/actions/workflows/cache.yml/badge.svg)](https://github.com/etu/nixconfig/actions/workflows/cache.yml)
[![Deploy 🚀](https://github.com/etu/nixconfig/actions/workflows/deploy.yml/badge.svg)](https://github.com/etu/nixconfig/actions/workflows/deploy.yml)
[![Format 🔎](https://github.com/etu/nixconfig/actions/workflows/format.yml/badge.svg)](https://github.com/etu/nixconfig/actions/workflows/format.yml)
[![Update ⬆️](https://github.com/etu/nixconfig/actions/workflows/update.yml/badge.svg)](https://github.com/etu/nixconfig/actions/workflows/update.yml)

# My NixOS configs

Personal NixOS configuration for multiple machines, managed with
[Nix Flakes](https://nixos.wiki/wiki/Flakes),
[home-manager](https://github.com/nix-community/home-manager),
[deploy-rs](https://github.com/serokell/deploy-rs) for remote deployments, and
[agenix](https://github.com/ryantm/agenix) for secret management. The flake
layout uses [blueprint](https://github.com/numtide/blueprint).

## Development

Enter the dev shell (defined in `devshell.nix`) for all tooling:

```sh
nix develop
# or, with direnv:
direnv allow
```

Common tasks are wrapped in the `Justfile` — run `just` to list all recipes.
Key groups: `build`, `deploy`, `format`, `format-check`, `updaters`, `nix`.

## Files and Directories in this repository

### `pubkeys.nix`

All SSH public keys for users and host systems — no secrets, just public keys.
Imported by `secrets-registry.nix` (to declare which hosts may decrypt each
secret) and by `modules/nixos/data/` (which exposes the keys as
`config.etu.data.pubkeys`).

### `flake.nix` and `flake.lock`

Main flake definition: inputs, blueprint wiring, deploy-rs nodes for the three
remotely-managed servers, and the live-iso package alias. All inputs follow
`nixpkgs` via `.follows` where possible. Key inputs: `agenix`, `blueprint`,
`catppuccin`, `deploy-rs`, `disko`, `emacs-overlay`, `home-manager`,
`impermanence`, `ip-failar-nu`, `nixos-hardware`, `nur`.

### `Justfile`

Task runner wrapping common `nix`, `deploy`, format, and update commands. Run
`just` to list everything. Notable recipes:

- `just build` / `just build-<hostname>` / `just build-all` — build hosts with `nom`
- `just deploy-server-main-elis` / `just deploy-server-sparv` / `just deploy-vps06`
- `just nix-fmt` / `just all-fmt-check` — format and lint checks
- `just update-all` — update flake inputs and all container image tags

### `hosts/`

#### `hosts/desktop-elis/`

Primary desktop for etu. Sway/Wayland graphical environment with development
tools, gaming (Steam, Minecraft, WoW), ham radio software, Flipper Zero
support, FDM 3D printing, and libvirtd virtualisation. ZFS with sanoid
snapshots. Deployed locally with `nixos-rebuild`.

#### `hosts/desktop-caroline/`

Desktop for Caroline. Sway/Wayland with creative tools (Blender, Inkscape,
LibreOffice), VSCodium, gaming (Steam, Minecraft, WoW), and FDM 3D printing.
ZFS with sanoid snapshots. Deployed locally with `nixos-rebuild`.

#### `hosts/laptop-private-elis/`

Private laptop (Lenovo T495). Deployed with `nixos-rebuild`. ZFS snapshots are
pushed from this system to `server-main-elis` via syncoid whenever online.

#### `hosts/laptop-private-caroline/`

Private laptop for Caroline. Similar to her desktop but without Steam. Creative
tools (Blender, Inkscape, LibreOffice) and VSCodium. Deployed locally with
`nixos-rebuild`.

#### `hosts/laptop-work-elis/`

Work laptop. Deployed with `nixos-rebuild`. ZFS snapshots are pushed from this
system to `server-main-elis` via syncoid whenever online.

#### `hosts/server-main-elis/`

Home file server, deployed using `deploy .#server-main-elis`. Also used as a
Nix build machine for the laptops. Primary ZFS backup target — pulls snapshots
from all desktops, laptops, server-sparv, and vps06 via syncoid.

Runs a large collection of services:

- **Home automation**: Home Assistant, Mosquitto MQTT, zwavejs2mqtt (all via
  Podman containers)
- **Media stack**: Jellyfin, NZBGet, Sonarr, Radarr, Lidarr, Bazarr
- **Web services**: Nextcloud, FreshRSS, Homepage dashboard, Nginx reverse proxy
- **Monitoring**: Beszel hub (collects from all three servers) + Beszel agent
- **Infrastructure**: Sanoid (ZFS snapshots), Syncoid (pulls backups from 7
  machines), Cloudflare DynDNS, smartd, libvirtd, ACME/Let's Encrypt
- **Scheduled tasks**: SVTPlay playlist downloader (daily), empty-dirs cleaner
  (hourly)

#### `hosts/server-sparv/`

On-location server for [speliarvika.se](http://speliarvika.se). Primarily a
game server host and LAN cache. Runs Valheim and Project Zomboid via Docker,
plus a whitelisted Minecraft server (NixOS service). Includes lancache
(monolithic + DNS) for LAN game cache. ZFS snapshots via sanoid. Monitoring
with Netdata (local dashboard) and Beszel agent. Deployed using
`deploy .#server-sparv`.

#### `hosts/vps06/`

VPS running public-facing services, deployed using `deploy .#vps06`:

- **Forgejo** — git forge at `git.elis.nu`
- **Matrix Synapse** — homeserver on `failar.nu`, with Element Web at
  `element.failar.nu` and an IRC bridge
- **Grafana** — dashboards at `grafana.elis.nu`
- **Matomo** — web analytics at `matomo.elis.nu`
- **ip.failar.nu** — custom service from the `ip-failar-nu` flake input
- **Nginx reverse proxy** — also forwards traffic to services hosted on
  `server-main-elis` (Nextcloud, FreshRSS, Home Assistant, Jellyfin, etc.)

#### `hosts/live-iso/`

If you're adventurous and want to run a clone of this configuration from a
live ISO, it can be built locally:

```sh
nix build github:etu/nixconfig#iso
```

### `modules/`

Reusable NixOS and home-manager modules with custom options.

**`modules/nixos/`** — system-level modules:
`base`, `data`, `development`, `games`, `graphical`, `services`, `theme`,
`user`, `work`.

**`modules/home/`** — user-level home-manager modules:
`alacritty`, `emacs`, `firefox`, `fish`, `flatpak-overrides`, `foot`,
`graphical-dotfiles`, `htop`, `kanshi`, `mako`, `sway`, `tmux`, `vscode`,
`waybar`.

### `secrets-registry.nix`, `secrets.nix` and `secrets/`

Secrets managed with [agenix](https://github.com/ryantm/agenix), which
[age](https://github.com/FiloSottile/age)-encrypts files using the SSH public
keys of authorised users and hosts. Encrypted `.age` files are safe to commit.

`secrets-registry.nix` is the **single source of truth**: each secret is
defined once with its file path, optional agenix options (`owner`, `path`,
`symlink`), and the list of host keys allowed to decrypt it. Both
`secrets.nix` (used by the agenix CLI) and `config.etu.data.ageModules`
(via `modules/nixos/data/`) are derived automatically from this registry.
Do not edit `secrets.nix` by hand.

### CI/CD (`.github/workflows/`)

Five workflows keep the repo healthy:

- **`format.yml`** — Nix fmt, yamllint, deadnix, statix checks on every push/PR
- **`pull_request.yaml`** — builds dev shell, runs flake check, builds all hosts
- **`cache.yml`** — builds and caches all hosts to Cachix on pushes to `main`
- **`deploy.yml`** — deploys the three remote servers via deploy-rs after a push to `main`
- **`update.yml`** — scheduled PRs to update flake inputs and container image tags
