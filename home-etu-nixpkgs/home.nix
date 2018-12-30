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
        cp ${./dotfiles/emacs/config.org} config.org
        ${pkgs.emacs}/bin/emacs --batch ./config.org -f org-babel-tangle
        mv config.el $out
      ''; }

    # Stupidterm
    { target = ".config/stupidterm.ini"; source = ./dotfiles/stupidterm.ini; }
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

  # Enable the dunst notification deamon
  services.dunst.enable = true;
  services.dunst.settings = {
    global = {
      # font = "";

      # Allow a small subset of html markup
      markup = "yes";
      plain_text = "no";

      # The format of the message
      format = "<b>%s</b>\\n%b";

      # Alignment of message text
      alignment = "center";

      # Split notifications into multiple lines
      word_wrap = "yes";

      # Ignore newlines '\n' in notifications.
      ignore_newline = "no";

      # Hide duplicate's count and stack them
      stack_duplicates = "yes";
      hide_duplicates_count = "yes";

      # The geometry of the window
      geometry = "420x50-15+49";

      # Shrink window if it's smaller than the width
      shrink = "no";

      # Don't remove messages, if the user is idle
      idle_threshold = 0;

      # Which monitor should the notifications be displayed on.
      monitor = 0;

      # The height of a single line. If the notification is one line it will be
      # filled out to be three lines.
      line_height = 3;

      # Draw a line of "separatpr_height" pixel height between two notifications
      separator_height = 2;

      # Padding between text and separator
      padding = 6;
      horizontal_padding = 6;

      # Define a color for the separator
      separator_color = "frame";

      # dmenu path
      dmenu = "${pkgs.rofi}/bin/rofi -dmenu -p dunst -theme glue_pro_blue";

      # Browser for opening urls in context menu.
      browser = "/run/current-system/sw/bin/firefox -new-tab";

      # Align icons left/right/off
      icon_position = "left";
      max_icon_size = 80;

      # Define frame size and color
      frame_width = 3;
      frame_color = "#8EC07C";
    };

    shortcuts = {
      close = "ctrl+space";
      close_all = "ctrl+shift+space";
    };

    urgency_low = {
      frame_color = "#3B7C87";
      foreground = "#3B7C87";
      background = "#191311";
      timeout = 4;
    };
    urgency_normal = {
      frame_color = "#5B8234";
      foreground = "#5B8234";
      background = "#191311";
      timeout = 6;
    };

    urgency_critical = {
      frame_color = "#B7472A";
      foreground = "#B7472A";
      background = "#191311";
      timeout = 8;
    };
  };

  services.flameshot.enable = true;
  services.pasystray.enable = true;
  services.network-manager-applet.enable = true;
}
