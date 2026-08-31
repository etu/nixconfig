_: {
  # Configure quickshell, run alongside waybar for extra widgets (OSD,
  # notification popups, sidebar, etc.).
  programs.quickshell = {
    enable = true;
    systemd.enable = true;

    activeConfig = "default";
    configs.default = ./config;
  };
}
