{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.emacs;

  myEmacsConfig = pkgs.writeText "config.el" (builtins.readFile ./emacs-files/base.el);
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
      (load-file "${myEmacsConfig}")

      ;; Set paths to things
      (setq gnuplot-program "${pkgs.gnuplot}/bin/gnuplot"
            phpcbf-executable "${pkgs.phpPackages.phpcbf}/bin/phpcbf"))

    ;;; emacs.el ends here
  '';

in {
  options = {
    my.emacs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enables emacs with the modules I want.
        '';
      };
      enableExwm = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    services.emacs.enable = true;
    services.emacs.package = (import ./emacs-files/elisp.nix { inherit pkgs; }).fromEmacsUsePackage {
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
      extraEmacsPackages = [ "myConfigInit" ] ++ optionals cfg.enableExwm [ "exwm" "desktop-environment" ];
    };
    services.emacs.defaultEditor = true;

    fonts.fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };
}
