{ ... }:
{
  # Set up kanshi (which kinda is an autorandr for wayland)
  services.kanshi = {
    enable = true;
    settings = [
      {
        profile.name = "undocked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "enable";
          }
        ];
      }
      {
        profile.name = "elis-desktop";
        profile.outputs = [
          {
            criteria = "LG Electronics LG SDQHD 402NTGY0Z759";
            mode = "2560x2880";
            position = "0,0";
          }
          {
            criteria = "LG Electronics LG SDQHD 402NTZN0Z757";
            mode = "2560x2880";
            position = "2560,110";
            transform = "270";
          }
        ];
      }
      {
        profile.name = "elis-docked";
        profile.outputs = [
          {
            criteria = "LG Electronics LG SDQHD 402NTGY0Z759";
            mode = "2560x2880";
            position = "0,0";
          }
          {
            criteria = "LG Electronics LG SDQHD 402NTZN0Z757";
            mode = "2560x2880";
            position = "2560,110";
            transform = "270";
          }
          {
            criteria = "eDP-1";
            status = "disable";
          }
        ];
      }
      {
        profile.name = "caroline-desktop";
        profile.outputs = [
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
      }
      {
        profile.name = "caroline-single-docked";
        profile.outputs = [
          {
            criteria = "Ancor Communications Inc ASUS PB278 C9LMTF095084";
            mode = "1920x1080";
            position = "0,0";
          }
          {
            criteria = "eDP-1";
            status = "disable";
          }
        ];
      }
    ];
  };
}
