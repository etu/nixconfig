# Find the hostname for default build target.

hostname := shell('hostname')

# Make a private default recipe to list recipes.
_default:
    @just --list

# nix flake show
[group('nix')]
flake-show:
    nix flake show

# nix flake check
[group('nix')]
flake-check :
    nix flake check

# Build the current host
[group('build')]
build:
    nom build '.#nixosConfigurations.{{ hostname }}.config.system.build.toplevel'

# Build the desktop-caroline host
[group('build')]
build-desktop-caroline:
    nom build '.#nixosConfigurations.desktop-caroline.config.system.build.toplevel'

# Build the desktop-elis host
[group('build')]
build-desktop-elis:
    nom build '.#nixosConfigurations.desktop-elis.config.system.build.toplevel'

# Build the laptop-private-caroline host
[group('build')]
build-laptop-private-caroline:
    nom build '.#nixosConfigurations.laptop-private-caroline.config.system.build.toplevel'

# Build the laptop-private-elis host
[group('build')]
build-laptop-private-elis:
    nom build '.#nixosConfigurations.laptop-private-elis.config.system.build.toplevel'

# Build the laptop-work-elis host
[group('build')]
build-laptop-work-elis:
    nom build '.#nixosConfigurations.laptop-work-elis.config.system.build.toplevel'

# Build the live-iso
[group('build')]
build-live-iso:
    nom build '.#iso'

# Build the server-main-elis host
[group('build')]
build-server-main-elis:
    nom build '.#nixosConfigurations.server-main-elis.config.system.build.toplevel'

# Build the server-sparv host
[group('build')]
build-server-sparv:
    nom build '.#nixosConfigurations.server-sparv.config.system.build.toplevel'

# Build the vps06 host
[group('build')]
build-vps06:
    nom build '.#nixosConfigurations.vps06.config.system.build.toplevel'

# Deploy the server-main-elis host
[group('deploy')]
deploy-server-main-elis:
    deploy --skip-checks --targets '.#server-main-elis'

# Deploy the server-sparv host
[group('deploy')]
deploy-server-sparv:
    deploy --skip-checks --targets '.#server-sparv'

# Deploy the vps06 host
[group('deploy')]
deploy-vps06:
    deploy --skip-checks --targets '.#vps06'

# Nix format
[group('format')]
nix-fmt:
    nix fmt

# Yaml format
[group('format')]
yaml-fmt:
    yamllint --strict --format github .

# Nix fmt check
[group('format-check')]
nix-fmt-check:
    nix fmt -- --ci

# Yaml fmt check
[group('format-check')]
yaml-fmt-check:
    yamllint --strict --format github .

# Deadnix fmt check
[group('format-check')]
deadnix-fmt-check:
    deadnix --fail hosts/ modules/ packages/

# Statix fmt check
[group('format-check')]
statix-fmt-check:
    statix check --config .statix.toml

# All fmt check
[group('format-check')]
all-fmt-check: nix-fmt-check yaml-fmt-check deadnix-fmt-check statix-fmt-check

# Update flake inputs
[group('updaters')]
update-flake:
    nix flake update

# Update home assistant container
[group('updaters')]
update-hass:
    sed -i -r 's#(ghcr.io/home-assistant/home-assistant):[0-9]{4}\.[0-9]{1,2}\.[0-9]{1,2}#\1:'`git ls-remote --tags 'https://github.com/home-assistant/core.git' | cut -d '/' -f 3 | grep -e '^20' | grep -v b | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

# Update zwavejs2mqtt container
[group('updaters')]
update-zwavejs2mqtt:
    sed -i -r 's#(zwavejs/zwavejs2mqtt):[1-9]+\.[0-9]+\.[0-9]+#\1:'`git ls-remote --tags 'https://github.com/zwave-js/zwavejs2mqtt.git' | cut -d 'v' -f 2 | grep -v '\^{}' | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

# Update mosquitto container
[group('updaters')]
update-mosquitto:
    sed -i -r 's#(eclipse-mosquitto):[1-9]+\.[0-9]+\.?[0-9]*#\1:'`git ls-remote --tags 'https://github.com/eclipse/mosquitto.git' | cut -d 'v' -f 2 | grep -v -e '\^{}' -e 'refs/tags' -e 'rc' | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

# Update vscode extensions
[group('updaters')]
update-vscode-extensions:
    nix run '.#vscodeGetLatestExtensions' github copilot-chat > modules/nixos/development/vscode/extensions/github-copilot-chat.nix
    nix run '.#vscodeGetLatestExtensions' Leathong openscad-language-support > modules/nixos/development/vscode/extensions/openscad.nix
    nix run '.#vscodeGetLatestExtensions' wongjn php-sniffer > modules/nixos/development/vscode/extensions/php-sniffer.nix
    nix run '.#vscodeGetLatestExtensions' Vue volar > modules/nixos/development/vscode/extensions/volar.nix
    nix run '.#vscodeGetLatestExtensions' joelwmale vscode-codeception > modules/nixos/development/vscode/extensions/vscode-codeception.nix
    nix run '.#vscodeGetLatestExtensions' kimgronqvist vscode-ido > modules/nixos/development/vscode/extensions/vscode-ido.nix
    nix run '.#vscodeGetLatestExtensions' ms-vscode vscode-speech > modules/nixos/development/vscode/extensions/vscode-speech.nix

# Update all
[group('updaters')]
update-all: update-flake update-hass update-zwavejs2mqtt update-mosquitto update-vscode-extensions
