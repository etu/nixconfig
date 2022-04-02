{ config, lib, ... }:
let
  cfg = config.my.home-manager;
in
{
  config = lib.mkIf cfg.enable {
    # Htop configurations
    home-manager.users.${config.my.user.username}.programs.htop = {
      enable = true;
      settings = {
        hide_userland_threads = true;
        hide_kernel_threads = true;
        highlight_base_name = true;
        shadow_other_users = true;
        show_program_path = true;
        tree_view = true;

        left_meters = [ "LeftCPUs" "Memory" "Swap" "ZFSARC" "ZFSCARC" ];
        left_meter_modes = [ 1 1 1 2 2 ];

        right_meters = [ "RightCPUs" "Tasks" "LoadAverage" "Uptime" "Battery" ];
        right_meter_modes = [ 1 2 2 2 2 ];
      };
    };
  };
}
