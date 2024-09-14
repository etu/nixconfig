{
  config,
  lib,
  ...
}: {
  options.etu.base.zfs = let
    options = param: prefix: {
      directories = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
        description = "Directories to pass to environment.persistence attribute for ${param} under ${prefix}";
      };
      files = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
        description = "Files to pass to environment.persistence attribute for ${param} under ${prefix}";
      };
    };
  in {
    enable = lib.mkEnableOption "Enable base zfs persistence settings";
    local = options "local" config.etu.localPrefix;
    system = options "system" config.etu.dataPrefix;
    user = options "user" config.etu.dataPrefix;
    root = options "root" config.etu.dataPrefix;
  };

  config = lib.mkIf config.etu.base.zfs.enable {
    environment.persistence.${config.etu.dataPrefix} = {
      # System persistence
      directories =
        [
          "/var/lib/nixos"
        ]
        ++ config.etu.base.zfs.system.directories;
      files =
        [
          "/etc/machine-id"
        ]
        ++ config.etu.base.zfs.system.files;

      # Root user persistence
      users.root = {
        inherit (config.users.users.root) home;
        inherit (config.etu.base.zfs.root) directories files;
      };

      # My user persistence
      users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
        inherit (config.etu.base.zfs.user) directories files;
      };
    };

    # Persistence for local files that may not be backed up
    environment.persistence.${config.etu.localPrefix} = {
      inherit (config.etu.base.zfs.local) directories files;
    };
  };
}
