{ config, lib, pkgs, ... }:
{
  options.my = {
    backup = {
      enable = lib.mkEnableOption "Enables backup related thingys";
      enableSanoid = lib.mkEnableOption "Enables snapshot creation";
      enableSyncoid = lib.mkEnableOption "Enables snapshot syncing";
    };
    common-cli.enable = lib.mkEnableOption "Enables my common CLI thingys";
    deploy-user.enable = lib.mkEnableOption "Enables my deploy user";
    emacs = {
      enable = lib.mkEnableOption "Enables emacs with the modules I want";
      enableWork = lib.mkEnableOption "Enables install of work related modules";
      package = lib.mkOption {
        type = lib.types.str;
        default = "default";
        defaultText = "default";
        description = "Which emacs package to use.";
      };
    };
    gaming.enable = lib.mkEnableOption "Enables gaming related thingys";
    gpg-utils.enable = lib.mkEnableOption "Enables smartcard and gpg related utils that I use";
    home-manager.enable = lib.mkEnableOption "Enables my home-manager config";
    nfsd.enable = lib.mkEnableOption "Enables nfsd and configures ports and stuff";
    spell.enable = lib.mkEnableOption "Enable and install aspell and hunspell with swedish and english dictionary";
    vbox.enable = lib.mkEnableOption "Enables vbox and related things I use";
  };
}
