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

      # Install g90 update fw for easy firmware updating on linux.
      (pkgs.buildGoModule {
        pname = "g90updatefw";
        version = "2021-11-22";

        src = pkgs.fetchFromGitHub {
          owner = "DaleFarnsworth";
          repo = "g90updatefw";
          rev = "fd69662a7ced939ab231c5d255984bb5393f2a68";
          sha256 = "sha256-8hOWx2WNL8N4j/20eVtvQ5+oHajzYufxvXcRziwxsj8=";
        };

        vendorSha256 = null;
      })
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
