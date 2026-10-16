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
`config.etu.data.pubkeys`). Mittens' key is authorized for root SSH access on
`server-sparv`. The `laptop-work-elis` SSH key is retained for a separately
managed device; this repository no longer configures that host. The
`macbook-air-elis` key is the user key on the nix-darwin MacBook Air M5 and is
part of `etu.computers` and the agenix `etu` key set.

### `flake.nix` and `flake.lock`

Main flake definition: inputs, blueprint wiring, deploy-rs nodes for the three
remotely-managed servers, and the live-iso package alias. Per-system outputs
are generated for `aarch64-darwin`, `aarch64-linux` and `x86_64-linux` only
(nixpkgs 26.11 dropped `x86_64-darwin`). All inputs follow
`nixpkgs` via `.follows` where possible. Key inputs: `agenix`, `blueprint`,
`catppuccin`, `deploy-rs`, `disko`, `emacs-overlay`, `home-manager`,
`impermanence`, `ip-failar-nu`, `nix-darwin`, `nixos-hardware`, `nur`.

### `Justfile`

Task runner wrapping common `nix`, `deploy`, format, and update commands. Run
`just` to list everything. Notable recipes:

- `just build` / `just build-<hostname>` / `just build-all` — build hosts with `nom`
  (`just build` builds the current host, using its nix-darwin config on macOS)
- `just deploy-server-main-elis` / `just deploy-server-sparv` / `just deploy-vps06`
- `just activate` — build and activate the current host locally (`darwin-rebuild`
  on macOS, `nixos-rebuild` on Linux, e.g. `laptop-private-elis`)
- `just nix-fmt` / `just all-fmt-check` — format and lint checks
- `just update-all` — update flake inputs and all container image tags

### `hosts/`

#### `hosts/laptop-private-elis/`

Private laptop (Lenovo ThinkPad P14s Gen 4). Deployed with `nixos-rebuild`. ZFS snapshots are
pushed from this system to `server-main-elis` via syncoid whenever online.

#### `hosts/macbook-air-elis/`

MacBook Air M5 (`aarch64-darwin`) managed with
[nix-darwin](https://github.com/nix-darwin/nix-darwin) via
`darwin-configuration.nix`, which blueprint exposes as
`darwinConfigurations.macbook-air-elis`. The host file only sets the
hostname, platform, state version and a few `etu.*` options; everything else
comes from `flake.darwinModules.default` (see `modules/darwin/` below): Nix
settings and caches (with weekly garbage collection and daily store
optimisation), dark mode, an auto-hiding Dock without recent apps, battery percentage and Bluetooth in the menu bar, Caps Lock remapped
to Control, faster key repeat, Touch ID for `sudo` (also inside tmux), the command line tools (including NUR tools),
Tailscale (`tailscaled` as a launchd daemon; log in once with
`sudo tailscale up`), fish (in `/etc/shells`; set as login shell once with
`chsh`), Emacs with its overlay and fonts, Claude Code, VS Code, OpenSCAD, the AeroSpace tiling
window manager (sway-like bindings on Alt, see `modules/darwin/graphical/aerospace`),
and the shared `emacs`, `fish`, `ghostty`, `git`, `htop` and `tmux`
home-manager modules (Ghostty uses the macOS `ghostty-bin` package there,
always starts fish and quits after its last window closes; git always signs
commits with the SSH key), ssh using the macOS ssh-agent with key
passphrases in the Keychain (loaded into the agent at login), and direnv with
nix-direnv.
User-specific home-manager settings live in
`users/elishirwing/home-configuration.nix` (zsh, extra fish abbreviations) and
are applied together with the system.
The emacs module uses the native macOS `emacs` build on darwin instead of
`emacs-pgtk`, and adds the Nix profiles to its PATH so `Emacs.app` launched
from Spotlight finds git and other tools instead of the Xcode stubs in
`/usr/bin`. Its `Emacs.app` starts Emacs as a separate process, like a
terminal would, so AeroSpace manages its windows. Bootstrap steps are in
the header of the configuration file; later changes are applied with
`just activate`. Built and cached in CI on a `macos-latest` (Apple Silicon)
runner alongside the NixOS hosts.

#### `hosts/server-main-elis/`

Home file server, deployed using `deploy .#server-main-elis`. Also used as a
Nix build machine for the laptops. Primary ZFS backup target — pulls snapshots
from configured backup sources via syncoid.

Runs a large collection of services:

- **Home automation**: Home Assistant, Mosquitto MQTT, zwavejs2mqtt (all via
  Podman containers)
- **Media stack**: Jellyfin, NZBGet, Sonarr, Radarr, Lidarr, Bazarr
- **Web services**: Nextcloud, FreshRSS, Homepage dashboard, Nginx reverse proxy
- **Monitoring**: Beszel hub (collects from all three servers) + Beszel agent
- **Infrastructure**: Sanoid (ZFS snapshots), Syncoid (pulls backups from
  configured systems), Cloudflare DynDNS, smartd, libvirtd, ACME/Let's Encrypt
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

**`modules/darwin/`** — nix-darwin modules, mirroring `modules/nixos/` and
its `etu.*` option names (so shared home modules reading `osConfig.etu.*`
work on macOS too): `base` (emacs, fish, htop, nix, packages, ssh, system defaults,
tailscale, tmux), `development` (claude-code, direnv, git, vscode), `graphical` (AeroSpace, fdm-printing,
Ghostty, theme fonts), `user`.

**`modules/home/`** — user-level home-manager modules:
`alacritty`, `dms-shell`, `emacs`, `firefox`, `fish`, `flatpak-overrides`,
`ghostty`, `graphical-dotfiles`, `htop`, `kanshi`, `sway`, `tmux`, `vscode`,
`voxtype`.
The Ghostty module disables the default `Ctrl+Enter` fullscreen toggle and
enables copy-on-select.

### `secrets-registry.nix`, `agenix-rules.nix` and `secrets/`

Secrets managed with [agenix](https://github.com/ryantm/agenix), which
[age](https://github.com/FiloSottile/age)-encrypts files using the SSH public
keys of authorised users and hosts. Encrypted `.age` files are safe to commit.

`secrets-registry.nix` is the **single source of truth**: each secret is
defined once with its file path, optional agenix options (`owner`, `path`,
`symlink`), and the list of host keys allowed to decrypt it. Both
`agenix-rules.nix` (used by the agenix CLI) and `config.etu.data.ageModules`
(via `modules/nixos/data/`) are derived automatically from this registry.
Do not edit `agenix-rules.nix` by hand.

### CI/CD (`.github/workflows/`)

Five workflows keep the repo healthy:

- **`format.yml`** — Nix fmt, yamllint, deadnix, statix checks on every push/PR
- **`pull_request.yaml`** — builds dev shell, runs flake check, builds all hosts
  (NixOS hosts on Linux runners, the MacBook on a macOS runner)
- **`cache.yml`** — builds and caches all hosts (including the MacBook) to Cachix on pushes to `main`
- **`deploy.yml`** — deploys the three remote servers via deploy-rs after a push to `main`
- **`update.yml`** — scheduled PRs to update flake inputs and container image tags
