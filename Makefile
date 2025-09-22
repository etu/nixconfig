#
# Helpers to build things
#
desktop-caroline:
	nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.desktop-caroline.config.system.build.toplevel

desktop-elis:
	nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.desktop-elis.config.system.build.toplevel

laptop-private-elis:
	nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.laptop-private-elis.config.system.build.toplevel

laptop-private-caroline:
	nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.laptop-private-caroline.config.system.build.toplevel

laptop-work-elis:
	nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.laptop-work-elis.config.system.build.toplevel

live-iso:
	nix run nixpkgs#nix-output-monitor -- build .#iso

server-main-elis:
	nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.server-main-elis.config.system.build.toplevel

server-sparv:
	nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.server-sparv.config.system.build.toplevel

vps06:
	nix run nixpkgs#nix-output-monitor -- build .#nixosConfigurations.vps06.config.system.build.toplevel

#
# Helpers to format the code
#
nix-fmt:
	@echo "Format nix files"
	nix fmt .

yaml-fmt:
	@echo "Format yaml files"
	nix run nixpkgs#yamllint -- --strict --format github .

#
# Helpers to check formatting of code
#
nix-fmt-check:
	@echo "Check nix file formatting ❄️"
	nix fmt . -- --check

yaml-fmt-check:
	@echo "Check yaml file formatting 📂"
	nix run nixpkgs#yamllint -- --strict --format github .

deadnix-fmt-check:
	@echo "Check deadnix file formatting ❄️"
	nix run nixpkgs#deadnix -- --fail hosts/ modules/ packages/

statix-fmt-check:
	@echo "Check statix file formatting ❄️"
	nix run nixpkgs#statix -- check --config .statix.toml

all-fmt-check: nix-fmt-check yaml-fmt-check deadnix-fmt-check statix-fmt-check

#
# Helpers to update containers
#
update-all: update-hass update-zwavejs2mqtt update-mosquitto update-vscode-extensions

update-hass:
	@echo "Updating to latest home assistant container"
	@sed -i -r 's#(ghcr.io/home-assistant/home-assistant):[0-9]{4}\.[0-9]{1,2}\.[0-9]{1,2}#\1:'`git ls-remote --tags 'https://github.com/home-assistant/core.git' | cut -d '/' -f 3 | grep -e '^20' | grep -v b | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

update-zwavejs2mqtt:
	@echo "Updating to latest zwavejs2mqtt container"
	@sed -i -r 's#(zwavejs/zwavejs2mqtt):[1-9]+\.[0-9]+\.[0-9]+#\1:'`git ls-remote --tags 'https://github.com/zwave-js/zwavejs2mqtt.git' | cut -d 'v' -f 2 | grep -v '\^{}' | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

update-mosquitto:
	@echo "Updating to latest mosquitto container"
	@sed -i -r 's#(eclipse-mosquitto):[1-9]+\.[0-9]+\.?[0-9]*#\1:'`git ls-remote --tags 'https://github.com/eclipse/mosquitto.git' | cut -d 'v' -f 2 | grep -v -e '\^{}' -e 'refs/tags' | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

update-vscode-extensions:
	@echo "Updating vscode extensions"
	@nix run .#vcodeGetLatestExtensions github copilot-chat > modules/development/vscode/extensions/github-copilot-chat.nix
	@nix run .#vcodeGetLatestExtensions Leathong openscad-language-support > modules/development/vscode/extensions/openscad.nix
	@nix run .#vcodeGetLatestExtensions wongjn php-sniffer > modules/development/vscode/extensions/php-sniffer.nix
	@nix run .#vcodeGetLatestExtensions Vue volar > modules/development/vscode/extensions/volar.nix
	@nix run .#vcodeGetLatestExtensions joelwmale vscode-codeception > modules/development/vscode/extensions/vscode-codeception.nix
	@nix run .#vcodeGetLatestExtensions kimgronqvist vscode-ido > modules/development/vscode/extensions/vscode-ido.nix
	@nix run .#vcodeGetLatestExtensions ms-vscode vscode-speech > modules/development/vscode/extensions/vscode-speech.nix
