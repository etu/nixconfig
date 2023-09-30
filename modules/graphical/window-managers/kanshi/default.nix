{
  config,
  lib,
  ...
}: {
  options.etu.graphical.window-managers.kanshi.enable = lib.mkEnableOption "Enable kanshi, kinda autorandr for wayland";

  config = lib.mkIf config.etu.graphical.window-managers.kanshi.enable {
    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Set up kanshi (which kinda is an autorandr for wayland)
      services.kanshi = {
        enable = true;
        profiles = {
          undocked.outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
            }
          ];
          elis-docked.outputs = [
            {
              criteria = "Samsung Electric Company LC49G95T H4ZN900853";
              mode = "5120x1440";
              position = "0,0";
            }
            {
              criteria = "eDP-1";
              status = "disable";
            }
          ];
          caroline-docked.outputs = [
            {
              criteria = "Ancor Communications Inc ASUS PB278 C9LMTF095084";
              mode = "2560x1440";
              position = "0,0";
            }
            {
              criteria = "Ancor Communications Inc ASUS PB278 CALMTF116261";
              mode = "2560x1440";
              position = "2560,0";
            }
            {
              criteria = "eDP-1";
              status = "disable";
            }
          ];
        };
      }; # END kanshi
    };
  };
}
