{
  config,
  lib,
  flake,
  pkgs,
  ...
}:
{
  options.etu.graphical.ghostty.enable = lib.mkEnableOption "Enable ghostty terminal emulator";

  config = lib.mkIf config.etu.graphical.ghostty.enable {
    # Enable the home-manager configuration for ghostty for my user.
    home-manager.users.${config.etu.user.username} =
      { config, ... }:
      {
        imports = [
          flake.homeModules.ghostty
        ];

        programs.ghostty = {
          # The official macOS build (the source build is Linux-only).
          package = pkgs.ghostty-bin;

          settings = {
            # Always start fish, regardless of the account's login shell.
            command = lib.getExe config.programs.fish.package;

            # Quit when the last window is closed, like on Linux. Otherwise
            # every instance started with `open -na Ghostty` (e.g. by
            # AeroSpace's Alt+Return) stays running in the Dock after its window
            # is closed.
            quit-after-last-window-closed = true;
          };
        };
      };
  };
}
