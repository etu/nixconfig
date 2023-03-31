#
# Helpers to build things
#
laptop-private-elis:
	nom build .#nixosConfigurations.laptop-private-elis.config.system.build.toplevel

laptop-work-elis:
	nom build .#nixosConfigurations.laptop-work-elis.config.system.build.toplevel

live-iso:
	nom build .#nixosConfigurations.live-iso.config.system.build.isoImage

pi-octonix:
	nom build .#nixosConfigurations.pi-octonix.config.system.build.toplevel

pi-octonix-sd-image:
	nom build .#nixosConfigurations.pi-octonix.config.system.build.sdImage
	@echo "sudo dd if=result/sd-image/*.img of=/dev/mmcblk0 bs=16M status=progress; sync"

server-main-elis:
	nom build .#nixosConfigurations.server-main-elis.config.system.build.toplevel

server-sparv:
	nom build .#nixosConfigurations.server-sparv.config.system.build.toplevel

vps04:
	nom build .#nixosConfigurations.vps04.config.system.build.toplevel

vps06:
	nom build .#nixosConfigurations.vps06.config.system.build.toplevel

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
# Helpers to update containers
#
update-all: update-hass update-zwavejs2mqtt update-mosquitto update-nzbget-exporter

update-hass:
	@echo "Updating to latest home assistant container"
	@sed -i -r 's#(ghcr.io/home-assistant/home-assistant):[0-9]{4}\.[0-9]{1,2}\.[0-9]{1,2}#\1:'`git ls-remote --tags 'https://github.com/home-assistant/core.git' | cut -d '/' -f 3 | grep -e '^20' | grep -v b | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

update-zwavejs2mqtt:
	@echo "Updating to latest zwavejs2mqtt container"
	@sed -i -r 's#(zwavejs/zwavejs2mqtt):[1-9]+\.[0-9]+\.[0-9]+#\1:'`git ls-remote --tags 'https://github.com/zwave-js/zwavejs2mqtt.git' | cut -d 'v' -f 2 | grep -v '\^{}' | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

update-mosquitto:
	@echo "Updating to latest mosquitto container"
	@sed -i -r 's#(eclipse-mosquitto):[1-9]+\.[0-9]+\.?[0-9]*#\1:'`git ls-remote --tags 'https://github.com/eclipse/mosquitto.git' | cut -d 'v' -f 2 | grep -v '\^{}' | sort -V | tail -n 1`'#' hosts/server-main-elis/services/hass.nix

update-nzbget-exporter:
	@echo "Updating to latest nzbget-exporter container"
	@sed -i -r 's#(frebib/nzbget-exporter):[0-9]+\.[0-9]+\.[0-9]+#\1:'`curl -s 'https://hub.docker.com/v2/namespaces/frebib/repositories/nzbget-exporter/tags' | jq '.results[1].name' | sed 's/"//g'`'#' hosts/server-main-elis/services/monitoring.nix
