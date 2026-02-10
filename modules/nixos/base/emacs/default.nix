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
      description = "This allows to add strings that gets added to the emacs config file.";
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

    # Allow unfree packages in home-manager as well
    etu.base.nix.allowUnfreeHome = [
      "intelephense"
      "copilot-language-server"
    ];

    # Install emacs icons symbols
    fonts.packages = [
      pkgs.emacs-all-the-icons-fonts
    ];

    # Configure emacs for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.emacs
      ];

      # Pass the emacs-overlay to home module so it can build the package
      _module.args = {
        inherit flake;
      };
    };

    # Enable persistence for Emacs.
    etu.base.zfs.user.directories = [
      ".config/github-copilot"
      ".local/share/emacs"
    ];
  };
}
