{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.emacs;

  # Create a file with my config without path substitutes, this just mash my
  # different config files together to one file.
  myEmacsConfigPlain = pkgs.writeText "config-unsubstituted.el" (
    (builtins.readFile ./emacs-files/base.el)
    + (builtins.readFile ./emacs-files/eshell.el)
  );

  # Run my config trough substituteAll to replace all paths with paths to
  # programs etc to have as my actual config file.
  myEmacsConfig = (pkgs.runCommand "config.el" (with pkgs; {
    inherit gnuplot gocode;
    phpcs = phpPackages.phpcs;
    phpcbf = phpPackages.phpcbf;
  }) ''
    substituteAll ${myEmacsConfigPlain} $out
  '');

  # Define init file for for emacs to read my config file.
  myEmacsInit = pkgs.writeText "init.el" ''
    ;;; emacs.el -- starts here
    ;;; Commentary:
    ;;; Code:

    ;; Increase the threshold to reduce the amount of garbage collections made
    ;; during startups.
    (let ((gc-cons-threshold (* 50 1000 1000))
          (gc-cons-percentage 0.6)
          (file-name-handler-alist nil))

      ;; Load config
      (load-file "${myEmacsConfig}"))

    ;;; emacs.el ends here
  '';

in {
  options.my.emacs = {
    enable = mkEnableOption "Enables emacs with the modules I want";
    enableExwm = mkEnableOption "Enables EXWM related modules";
    enableWork = mkEnableOption "Enables install of work related modules";
    package = mkOption {
      type = types.package;
      default = pkgs.emacs;
      defaultText = "pkgs.emacs";
      description = "Which emacs package to use";
    };
  };

  config = mkIf cfg.enable {
    # Import the emacs overlay from nix community to get the latest
    # and greatest packages.
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];

    services.emacs.enable = true;
    services.emacs.package = (pkgs.emacsWithPackagesFromUsePackage {
      package = cfg.package;

      # Config to parse, use my built config from above
      config = builtins.readFile myEmacsConfig;

      # Package overrides
      override = epkgs: epkgs // {
        # Add my config initializer as an emacs package
        myConfigInit = (pkgs.runCommand "default.el" {} ''
          mkdir -p  $out/share/emacs/site-lisp
          cp ${myEmacsInit} $out/share/emacs/site-lisp/default.el
        '');

        # Override nix-mode source
        nix-mode = epkgs.nix-mode.overrideAttrs (oldAttrs: {
          src = builtins.fetchTarball {
            url = https://github.com/nixos/nix-mode/archive/master.tar.gz;
          };
        });
      };

      # Extra packages to install
      extraEmacsPackages = epkgs: (
        # Install my config file as a module
        # Also optinoally install exwm deps
        [ epkgs.myConfigInit ] ++ optionals cfg.enableExwm [
          epkgs.exwm epkgs.desktop-environment
        ] ++ optionals cfg.enableExwm [
          epkgs.es-mode epkgs.vcl-mode
        ]);
    });
    services.emacs.defaultEditor = true;

    fonts.fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };
}
