{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.development.rtl-sdr.enable = lib.mkEnableOption "Enable development rtl-sdr settings";

  config = lib.mkIf config.etu.development.rtl-sdr.enable {
    hardware.rtl-sdr.enable = true;

    # Install rtl-sdr utils using home manager.
    etu.user.extraUserPackages = [
      pkgs.cubicsdr
      pkgs.rtl-sdr
      pkgs.rtl_433
    ];

    # Define extra groups for user.
    etu.user.extraGroups = [ "plugdev" ];
  };
}
