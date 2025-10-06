_: {
  # Install tmux
  programs.tmux.enable = true;
  programs.tmux.clock24 = true;

  # Import config file
  home.file.".tmux.conf".source = ./tmux.conf;
}
