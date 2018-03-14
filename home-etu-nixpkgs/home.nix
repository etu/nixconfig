{ pkgs, ... }:

{
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;

  home.file = [
    # Nano config
    {
      target = ".nanorc";
      text = ''
      set constantshow	    # Show linenumbers -c as default
      '';
    }

    # Mpv config file
    {
      target = ".config/mpv/mpv.conf";
      text = ''
      # Don't show images embedded in music files
      no-audio-display
      '';
    }

    # Tmux config
    { target = ".tmux.conf"; source = ./dotfiles/tmux.conf; }

    # Fish config
    { target = ".config/fish/config.fish"; source = ./dotfiles/fish/config.fish; }

    # Fish functions
    { target = ".config/fish/functions"; source = ./dotfiles/fish/functions; }

    # Git config files
    { target = ".gitconfig"; source = ./dotfiles/git/gitconfig; }
    { target = ".gitconfig_work"; source = ./dotfiles/git/gitconfig_work; }
    { target = ".gitignore_global"; source = ./dotfiles/git/gitignore_global; }

    # Emacs config
    { target = ".emacs"; source = ./dotfiles/emacs/emacs.el; }
    { target = ".config/emacs/config.org"; source = ./dotfiles/emacs/config.org; }

    # Htop
    { target = ".config/htop/htoprc"; source = ./dotfiles/htop/htoprc; }

    # Stupidterm
    { target = ".config/stupidterm.ini"; source = ./dotfiles/stupidterm.ini; }
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs._0blayout
      epkgs.anzu
      epkgs.column-enforce-mode
      epkgs.company
      epkgs.company-flx
      epkgs.company-go
      epkgs.company-jedi
      epkgs.company-php
      epkgs.company-restclient
      epkgs.company-statistics
      epkgs.diff-hl
      epkgs.es-mode
      epkgs.eyebrowse
      epkgs.fish-mode
      epkgs.flycheck
      epkgs.geben
      epkgs.gnuplot
      epkgs.go-mode
      epkgs.guide-key
      epkgs.helm
      epkgs.helm-ag
      epkgs.helm-fuzzier
      epkgs.helm-projectile
      epkgs.htmlize
      epkgs.magit
      epkgs.markdown-mode
      epkgs.nix-mode
      epkgs.php-mode
      epkgs.phpcbf
      epkgs.restclient
      epkgs.scss-mode
      epkgs.smooth-scrolling
      epkgs.toml-mode
      epkgs.use-package
      epkgs.vcl-mode
      epkgs.web-mode
      epkgs.webpaste
      epkgs.yaml-mode
      epkgs.yasnippet
      epkgs.zerodark-theme
    ];
  };

  programs.git = {
    enable = true;
    userName = "";
    userEmail = "";
  };

  programs.browserpass.enable = true;

  manual.manpages.enable = false;
}
