{ config, lib, ... }:

{
  options.etu.base.zfs = let
    options = param: {
      directories = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Directories to pass to environment.persistence attribute for ${param} under ${config.etu.dataPrefix}";
      };
      files = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Files to pass to environment.persistence attribute for ${param} under ${config.etu.dataPrefix}";
      };
    };
  in {
    enable = lib.mkEnableOption "Enable base zfs persistence settings";
    system = options "system";
    user = options "user";
    root = options "root";
  };

  config = lib.mkIf config.etu.base.zfs.enable {
    environment.persistence.${config.etu.dataPrefix} = {
      # System persistence
      directories = config.etu.base.zfs.system.directories;
      files = [
        "/etc/machine-id"
      ] ++ config.etu.base.zfs.system.files;

      # Root user persistence
      users.root = {
        home = config.users.users.root.home;
        directories = config.etu.base.zfs.root.directories;
        files = config.etu.base.zfs.root.files;
      };

      # My user persistence
      users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
        directories = config.etu.base.zfs.user.directories;
        files = config.etu.base.zfs.user.files;
      };
    };
  };
}
