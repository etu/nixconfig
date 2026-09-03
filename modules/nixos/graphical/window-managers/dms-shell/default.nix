{
  config,
  lib,
  inputs,
  flake,
  ...
}:
{
  options.etu.graphical.window-managers.dms-shell.enable =
    lib.mkEnableOption "Enable DankMaterialShell, a Quickshell-based Material 3 desktop shell";

  config = lib.mkIf config.etu.graphical.window-managers.dms-shell.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        inputs.dms.homeModules.dank-material-shell
        flake.homeModules.dms-shell
      ];
    };

    # Persist the first-launch and changelog markers so dms-shell doesn't
    # show its welcome greeter/changelog dialog again after every reboot.
    # Unlike .firstlaunch, the changelog marker has no "existing user"
    # fallback in dms-shell itself if the marker is missing, so this is
    # the one that actually mattered. Its filename is version-pinned
    # (.changelog-<currentVersion> in ChangelogService.qml), so a future
    # dms-shell upgrade will show the changelog once for the new version
    # -- that's intended, not a regression.
    etu.base.zfs.user.files = [
      ".config/DankMaterialShell/.firstlaunch"
      ".config/DankMaterialShell/.changelog-1.6"
    ];
  };
}
