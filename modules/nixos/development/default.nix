{
  config,
  lib,
  ...
}:
{
  imports = [
    ./direnv
    ./flipper-zero
    ./git
    ./php
    ./rtl-sdr
    ./vscode
  ];

  options.etu.development.enable = lib.mkEnableOption "Enable development settings";

  config = lib.mkIf config.etu.development.enable {
    etu = {
      development.direnv.enable = lib.mkDefault true;
      development.git.enable = lib.mkDefault true;
      development.php.enable = lib.mkDefault true;
      development.rtl-sdr.enable = lib.mkDefault true;
      development.vscode.enable = lib.mkDefault true;

      # Define extra groups for user.
      user.extraGroups = [ "dialout" ];
    };
  };
}
