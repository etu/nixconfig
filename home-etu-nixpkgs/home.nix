{ pkgs, ... }:

{
  # Emacs config
  home.file.".emacs".source = ./dotfiles/emacs/emacs.el;
  home.file.".config/emacs/config.org".source = ./dotfiles/emacs/config.org;

  home.packages = [
    pkgs.pass
    pkgs.stow
    pkgs.kdeconnect
    pkgs.gnome3.gnome-tweak-tool
    pkgs.gnome3.gnome-shell-extensions
  ];

  programs.firefox = {
    enable = true;
    enableAdobeFlash = false;
  };

  programs.browserpass.enable = true;

  programs.git = {
    enable = true;
    userName = "Elis Axelsson";
    userEmail = "elis.axelsson@gmail.com";
    signing.key = "67FE98F28C44CF221828E12FD57EFA625C9A925F";
    extraConfig = ''
    [push]
    default = simple

    [color]
    diff = auto
    status = auto
    branch = auto

    [core]
    excludesfile = ~/.gitignore_global
    '';
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.magit
      epkgs.zerodark-theme
      epkgs.webpaste
      epkgs.helm
      epkgs.helm-projectile
      epkgs.helm-fuzzier
      epkgs.company
      epkgs.company-flx
      epkgs.company-statistics
      epkgs.company-restclient
      epkgs.company-php
      epkgs.company-go
      epkgs.fish-mode
      epkgs.go-mode
      epkgs.php-mode
      epkgs.scss-mode
      epkgs.web-mode
      epkgs.markdown-mode
      epkgs.yaml-mode
      epkgs.restclient
      epkgs.anzu
      epkgs.flycheck
      epkgs.eyebrowse
      epkgs.yasnippet
      epkgs.guide-key
      epkgs.smooth-scrolling
      epkgs.diff-hl
      epkgs.column-enforce-mode
      epkgs.geben
      epkgs.nyan-mode
      epkgs.htmlize
      epkgs.gnuplot
      pkgs.emacs-all-the-icons-fonts
    ];
  };

  manual.manpages.enable = false;
}
