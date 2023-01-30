{
  config,
  lib,
  pkgs,
  ...
}: let
  base = {
    programs.htop = {
      enable = true;
      settings = {
        hide_userland_threads = true;
        hide_kernel_threads = true;
        highlight_base_name = true;
        shadow_other_users = true;
        show_program_path = false;
        tree_view = true;

        left_meters = ["LeftCPUs" "Memory" "Swap" "ZFSARC" "ZFSCARC"];
        left_meter_modes = [1 1 1 2 2];

        right_meters = ["RightCPUs" "Tasks" "LoadAverage" "Uptime" "Battery"];
        right_meter_modes = [1 2 2 2 2];
      };
    };
  };
in {
  options.etu.base.htop.enable = lib.mkEnableOption "Enable base htop settings";

  config = lib.mkIf config.etu.base.htop.enable {
    # Always install htop as well as having it enabled in home-manager.
    environment.systemPackages = [pkgs.htop];

    # Configure htop for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable base;

    # Configure htop for root users home-manager.
    home-manager.users.root = base;
  };
}
