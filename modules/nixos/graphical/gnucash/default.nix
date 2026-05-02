{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.graphical.gnucash.enable = lib.mkEnableOption "Enable graphical gnucash settings";

  config = lib.mkIf config.etu.graphical.gnucash.enable {
    # Install gnucash using home-manager, wrapped to use Swedish locale.
    etu.user.extraUserPackages = [
      (pkgs.symlinkJoin {
        name = "gnucash";
        paths = [ pkgs.gnucash ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/gnucash --set LANG sv_SE.UTF-8
        '';
      })
    ];

    # Enable persistence for gnucash related files.
    etu.base.zfs.user.directories = [
      ".config/gnucash"
      ".local/share/gnucash"
    ];
  };
}
