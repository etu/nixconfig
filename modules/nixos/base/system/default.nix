{ config, lib, pkgs, ... }:
{
  # Set the nixpkgs inputs path as channel in the NIX_PATH variable.
  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];

  # Enable experimental features in the nix command to make nix search work.
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Set system state version.
  system.stateVersion = config.etu.stateVersion;

  # Display a diff of installed packages on system activation.
  system.activationScripts.diff = {
    supportsDryActivation = true;
    text = ''
      NO_FORMAT="\033[0m"
      F_BOLD="\033[1m"
      C_LIME="\033[38;5;10m"

      if test -e /run/current-system; then
        echo -e "''${F_BOLD}''${C_LIME}==> diff to current-system ''${NO_FORMAT}"
        ${pkgs.nvd}/bin/nvd --nix-bin-dir=${config.nix.package}/bin diff /run/current-system "$systemConfig"
      fi
    '';
  };

  # Enable doas.
  security.doas.enable = true;

  # Set backup file extensions for conflicts on home manager activation.
  home-manager.backupFileExtension = "backup";

  # Set state version for my users home-manager (if it's enabled).
  home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
    home.stateVersion = config.etu.stateVersion;
  };

  # Set state version for root users home-manager.
  home-manager.users.root.home.stateVersion = config.etu.stateVersion;
}
