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

    # Git config file for work
    { target = ".config/git/work_config"; source = ./dotfiles/git/gitconfig_work; }

    # Emacs config
    { target = ".emacs"; source = ./dotfiles/emacs/emacs.el; }
    { target = ".config/emacs/config.el";
      source = pkgs.runCommand "config.el" {} ''
        cp ${./dotfiles/emacs/config.org} config.org &&
          ${pkgs.emacs}/bin/emacs --batch ./config.org -f org-babel-tangle &&
          mv config.el $out
      ''; }

    # Stupidterm
    { target = ".config/stupidterm.ini"; source = ./dotfiles/stupidterm.ini; }

    # Direnvrc
    { target = ".direnvrc"; source = ./dotfiles/direnvrc.sh; }
  ];

  programs.git = {
    enable = true;
    userName = "Elis Hirwing";
    userEmail = "elis@hirwing.se";

    signing = {
      key = "67FE98F28C44CF221828E12FD57EFA625C9A925F";
      signByDefault = true;
    };

    ignores = [ ".ac-php-conf.json" ];

    includes = [
      { condition = "gitdir:~/tvnu/"; path = "~/.config/git/work_config"; }
    ];
  };

  programs.browserpass.enable = true;

  # Htop configurations
  programs.htop = {
    enable = true;
    hideUserlandThreads = true;
    highlightBaseName = true;
    shadowOtherUsers = true;
    showProgramPath = false;
    treeView = true;
    meters = {
      left = [
        { kind = "LeftCPUs";   mode = 1; }
        { kind = "Memory";     mode = 1; }
        { kind = "Swap";       mode = 1; }
      ];
      right = [
        { kind = "RightCPUs";   mode = 1; }
        { kind = "Tasks";       mode = 2; }
        { kind = "LoadAverage"; mode = 2; }
        { kind = "Uptime";      mode = 2; }
      ];
    };
  };

  # GTK theme configs
  gtk.enable = true;
  gtk.gtk3.extraConfig = {
    gtk-application-prefer-dark-theme = 1;
  };
}
