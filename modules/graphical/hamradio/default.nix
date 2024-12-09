{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.hamradio.enable = lib.mkEnableOption "Enable graphical hamradio settings";

  config = lib.mkIf config.etu.graphical.hamradio.enable {
    # Install the software needed for operating FT-8 with my Xiegu G90.
    etu.user.extraUserPackages = [
      pkgs.flrig # flrig to connect to the radio
      pkgs.wsjtx # wsjtx to run FT-8
      pkgs.nur.repos.etu.g90updatefw # Install g90 update fw for easy firmware updating on linux.
    ];

    etu.base.zfs.user.files = [
      # Enable persistence for files for flrig.
      ".flrig/flrig.prefs"
      ".flrig/Xiegu-G90.prefs"

      # Enable persistence for files for WSJTX.
      ".config/WSJT-X.ini"
    ];

    etu.base.zfs.user.directories = [
      # Enable persistence for directories for WSJTX.
      ".local/share/WSJT-X/"
    ];
  };
}
