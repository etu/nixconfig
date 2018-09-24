{ config, lib, pkgs, ... }:

with lib;

let
 cfg = config.my.emacs;

 myEmacs = pkgs.emacs.override {};
 myEmacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
 myEmacsConfig = pkgs.writeText "default.el" ''
;;; default.el -- Global config starts here

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; default.el ends here
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
    };
  };

  config = mkIf cfg.enable {
    services.emacs.enable = true;
    services.emacs.package = (myEmacsWithPackages (epkgs: with epkgs; [
      (pkgs.runCommand "default.el" {} ''
        mkdir -p $out/share/emacs/site-lisp
        cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
      '')
      _0blayout
      anzu
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
    ]));
    services.emacs.defaultEditor = true;

    fonts.fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };
}
