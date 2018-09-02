{ config, lib, pkgs, ... }:

with lib;

let
 cfg = config.my.emacs;

 myEmacs = pkgs.emacs.override {};
 myEmacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
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
      es-mode
      eyebrowse
      fish-mode
      flycheck
      geben
      gnuplot
      go-mode
      guide-key
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
      use-package
      vcl-mode
      web-mode
      webpaste
      yaml-mode
      yasnippet
      zerodark-theme
    ]));
    services.emacs.defaultEditor = true;

    fonts.fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };
}
