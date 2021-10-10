update-all: update-nixpkgs update-niv update-intelephense

update-nixpkgs:
	cd nix/nixos-unstable/ && git pull origin nixos-unstable

update-niv:
	niv update

update-intelephense:
	./nix/packages/vscode-intelephense/update.sh
