{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.emacs;

  myEmacs = pkgs.emacs.override {};
  myEmacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
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
      (load-file "${myEmacsConfig}"))
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
    services.emacs.package = (myEmacsWithPackages (epkgs: with epkgs; ([
      (pkgs.runCommand "default.el" {} ''
        mkdir -p $out/share/emacs/site-lisp
        cp ${myEmacsInit} $out/share/emacs/site-lisp/default.el
      '')
      _0blayout
      anzu
      centimacro
      column-enforce-mode
      company
      company-flx
      company-go
      company-jedi
      company-php
      company-restclient
      company-statistics
      diff-hl
      direnv
      dracula-theme
      es-mode
      eyebrowse
      fish-mode
      flycheck
      geben
      gnuplot
      go-mode
      helm
      helm-ag
      helm-fuzzier
      helm-projectile
      htmlize
      magit
      markdown-mode
      nix-mode
      php-mode
      phpcbf
      restclient
      scss-mode
      smooth-scrolling
      telephone-line
      use-package
      vcl-mode
      web-mode
      webpaste
      which-key
      yaml-mode
      yasnippet
    ] ++ (lib.optional cfg.enableExwm (with epkgs; [
      exwm
      desktop-environment
    ])))));
    services.emacs.defaultEditor = true;

    fonts.fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };
}
