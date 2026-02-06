{
  config,
  lib,
  ...
}:
{
  options.etu.base.zfs =
    let
      options = param: prefix: {
        directories = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Directories to pass to environment.persistence attribute for ${param} under ${prefix}";
        };
        files = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Files to pass to environment.persistence attribute for ${param} under ${prefix}";
        };
      };
    in
    {
      enable = lib.mkEnableOption "Enable base zfs persistence settings";
      local = options "local" config.etu.localPrefix;
      localUser = options "localUser" config.etu.localPrefix;
      system = options "system" config.etu.dataPrefix;
      user = options "user" config.etu.dataPrefix;
      root = options "root" config.etu.dataPrefix;
    };

  config = lib.mkIf config.etu.base.zfs.enable {
    environment.etc."machine-id".source = "${config.etu.dataPrefix}/etc/machine-id";

    environment.persistence.${config.etu.dataPrefix} = {
      # System persistence
      inherit (config.etu.base.zfs.system) files;
      directories = [
        "/var/lib/nixos"
      ]
      ++ config.etu.base.zfs.system.directories;

      # Root user persistence
      users.root = {
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

      # Local user persistence
      users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
        inherit (config.etu.base.zfs.localUser) directories files;
      };
    };
  };
}
