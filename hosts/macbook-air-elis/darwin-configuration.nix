# MacBook Air M5 managed with nix-darwin.
#
# Initial bootstrap (Nix must already be installed):
# 1. Move away the files nix-darwin wants to manage:
#    $ sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
# 2. Activate the configuration:
#    $ sudo nix --extra-experimental-features 'nix-command flakes' run nix-darwin -- switch --flake .#macbook-air-elis
#
# Later deployments:
# $ just activate
{ flake, ... }:
{
  imports = [
    # Import my default modules
    flake.darwinModules.default
  ];

  nixpkgs.hostPlatform = "aarch64-darwin";

  # Set the hostname so darwin-rebuild picks this configuration by default.
  networking.hostName = "macbook-air-elis";
  networking.computerName = "macbook-air-elis";
  networking.localHostName = "macbook-air-elis";

  # My module settings
  etu = {
    user.username = "elishirwing";

    # Use zoxide for cd in fish, like on laptop-private-elis.
    base.fish.enableUserZoxideCd = true;
    development.claude-code.enable = true;
    graphical.theme.enable = true;
  };

  # Used for backwards compatibility, read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
