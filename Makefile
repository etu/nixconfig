update-all: update-nixpkgs update-niv update-firefox-extensions update-hass update-zwavejs2mqtt update-mosquitto update-nzbget-exporter update-chalet

update-nixpkgs:
	@echo "Updating nixpkgs to latest nixos-unstable"
	@cd nix/nixos-unstable/ && git pull origin nixos-unstable

update-niv:
	@echo "Updating niv dependencies"
	@niv update agenix
	@niv update emacs-overlay
	@niv update flummbot
	@niv update home-manager
	@niv update impermanence
	@niv update ip-failar-nu
	@niv update llr
	@niv update mkvcleaner
	@niv update nixos-hardware
	@niv update nixus

update-firefox-extensions:
	@echo "Updating firefox extensions"
	@nix-shell modules/graphical/firefox/generate.py

update-hass:
	@echo "Updating to latest home assistant container"
	@sed -i -r 's#(ghcr.io/home-assistant/home-assistant):[0-9]{4}\.[0-9]{1,2}\.[0-9]{1,2}#\1:'`git ls-remote --tags 'https://github.com/home-assistant/core.git' | cut -d '/' -f 3 | grep -e '^20' | grep -v b | sort -V | tail -n 1`'#' hosts/home-server/services/hass.nix

update-zwavejs2mqtt:
	@echo "Updating to latest zwavejs2mqtt container"
	@sed -i -r 's#(zwavejs/zwavejs2mqtt):[1-9]+\.[0-9]+\.[0-9]+#\1:'`git ls-remote --tags 'https://github.com/zwave-js/zwavejs2mqtt.git' | cut -d 'v' -f 2 | grep -v '\^{}' | sort -V | tail -n 1`'#' hosts/home-server/services/hass.nix

update-mosquitto:
	@echo "Updating to latest mosquitto container"
	@sed -i -r 's#(eclipse-mosquitto):[1-9]+\.[0-9]+\.?[0-9]*#\1:'`git ls-remote --tags 'https://github.com/eclipse/mosquitto.git' | cut -d 'v' -f 2 | grep -v '\^{}' | sort -V | tail -n 1`'#' hosts/home-server/services/hass.nix

update-nzbget-exporter:
	@echo "Updating to latest nzbget-exporter container"
	@sed -i -r 's#(frebib/nzbget-exporter):[0-9]+\.[0-9]+\.[0-9]+#\1:'`curl 'https://hub.docker.com/v2/namespaces/frebib/repositories/nzbget-exporter/tags' | jq '.results[1].name' | sed 's/"//g'`'#' hosts/home-server/services/monitoring.nix

update-chalet:
	@echo "Update chalet dependencies"
	@nix-shell modules/work/packages/chalet/update.sh
