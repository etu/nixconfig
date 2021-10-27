{ lib, pkgs, ... }:
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
      enableExwm = lib.mkEnableOption "Enables EXWM config and graphical environment";
      enableWork = lib.mkEnableOption "Enables install of work related modules";
      package = lib.mkOption {
        type = lib.types.str;
        default = "default";
        defaultText = "default";
        description = "Which emacs package to use.";
      };
    };
    fonts = {
      size = lib.mkOption {
        type = lib.types.int;
        default = 10;
        description = "Default font size";
      };
      biggerSize = lib.mkOption {
        type = lib.types.int;
        default = 13;
        description = "Default font size";
      };
      monospace = lib.mkOption {
        type = lib.types.str;
        default = "JetBrainsMono";
        description = "Which default font to use";
      };
      normal = lib.mkOption {
        type = lib.types.str;
        default = "DejaVu Sans";
        description = "Default non monospace font to use";
      };
    };
    gaming.enable = lib.mkEnableOption "Enables gaming related thingys";
    gpg-utils.enable = lib.mkEnableOption "Enables smartcard and gpg related utils that I use";
    home-manager.enable = lib.mkEnableOption "Enables my home-manager config";
    nfsd.enable = lib.mkEnableOption "Enables nfsd and configures ports and stuff";
    spell.enable = lib.mkEnableOption "Enable and install aspell and hunspell with swedish and english dictionary";
    sway = {
      enable = lib.mkEnableOption "Enables sway and auto login for my user";
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.sway;
        description = "Which sway package to use";
      };
    };
    user = {
      enable = lib.mkEnableOption "Enables my user";
      uid = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = 1000;
        description = "My user id for this system.";
      };
      username = lib.mkOption {
        type = lib.types.str;
        default = "etu";
        description = "My username for this system.";
      };
      realname = lib.mkOption {
        type = lib.types.str;
        default = "Elis Hirwing";
        description = "My realname for this system.";
      };
      email = lib.mkOption {
        type = lib.types.str;
        default = "elis@hirwing.se";
        description = "My email for this system.";
      };
      workEmail = lib.mkOption {
        type = lib.types.str;
        default = "elis.hirwing@schibsted.com";
        description = "My email for this system.";
      };
      signingKey = lib.mkOption {
        type = lib.types.str;
        default = "67FE98F28C44CF221828E12FD57EFA625C9A925F";
        description = "My public signing key for this system.";
      };
      extraGroups = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
      extraAuthorizedKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Additional authorized keys.";
      };
      persistent = {
        homeDir = lib.mkOption {
          type = lib.types.str;
          default = "/persistent/home/etu";
          description = "Location of persistent home files.";
        };
        extraFiles = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
        };
        extraDirectories = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
        };
      };
    };
    vbox.enable = lib.mkEnableOption "Enables vbox and related things I use";
  };
}
