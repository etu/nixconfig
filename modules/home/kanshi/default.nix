_: {
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
    ];
  };
}
