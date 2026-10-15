{
  config,
  flake,
  inputs,
  lib,
  pkgs,
  ...
}:
{
  options.etu.base.emacs = {
    enable = lib.mkEnableOption "Enable base emacs settings";
    extraConfig = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "This allows adding strings that get added to the emacs config file.";
    };
  };

  config = lib.mkIf config.etu.base.emacs.enable {
    # Import the emacs overlay from nix community to get the latest
    # and greatest packages.
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlay
    ];

    # Allow to install intelephense which is an unfree package.
    etu.base.nix.allowUnfree = [
      "intelephense"
      "copilot-language-server"
    ];

    # Install emacs icons symbols
    fonts.packages = [
      pkgs.emacs-all-the-icons-fonts
    ];

    # Configure emacs for my users home-manager.
    home-manager.users.${config.etu.user.username}.imports = [
      flake.homeModules.emacs
    ];
  };
}
