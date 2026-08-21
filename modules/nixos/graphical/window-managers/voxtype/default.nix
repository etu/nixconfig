{
  config,
  lib,
  flake,
  ...
}:
{
  options.etu.graphical.window-managers.voxtype.enable =
    lib.mkEnableOption "Enable voxtype, a push-to-talk speech-to-text daemon";

  config = lib.mkIf config.etu.graphical.window-managers.voxtype.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.voxtype
      ];
    };

    # Persist downloaded whisper models, no need to back these up since they
    # can be redownloaded.
    etu.base.zfs.localUser.directories = [
      ".local/share/voxtype/models"
    ];
  };
}
