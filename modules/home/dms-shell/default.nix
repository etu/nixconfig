_: {
  # Configure DankMaterialShell, a Quickshell-based Material 3 desktop
  # shell (bar, launcher, notifications, etc).
  programs.dank-material-shell = {
    enable = true;

    systemd.enable = true;
    systemd.restartIfChanged = true;

    # System monitoring widgets are powered by dgop, which is built into
    # the dms daemon itself and needs no extra package.
    enableSystemMonitoring = true;

    settings = {
      firstDayOfWeek = 1; # Week starts on Mondays
      showWeekNumber = true; # Show week numbers
      clockFormat = "24h"; # Military time
      showSeconds = true; # With seconds
      showWorkspaceIndex = true; # Workspace numbers in workspace switcher
      clockDateFormat = "yyyy-MM-dd"; # Date format in clock widget
      lockDateFormat = "yyyy-MM-dd"; # Lock date format
      useAutoLocation = true; # Location detection for weather
      screenPreferences.wallpaper = [ ]; # Disable wallpaper

      barConfigs = [
        {
          id = "default";
          name = "Main Bar";
          enabled = true;
          position = 0;
          screenPreferences = [ "all" ];
          showOnLastDisplay = true;
          leftWidgets = [
            "idleInhibitor"
            "cpuUsage"
            "memUsage"
            "cpuTemp"
            "battery"
            "network_speed_monitor"
          ];
          centerWidgets = [
            "workspaceSwitcher"
          ];
          rightWidgets = [
            "privacyIndicator"
            "weather"
            "clock"
            "clipboard"
            "notificationButton"
            "systemTray"
            "controlCenterButton"
          ];
          attachToScreenEdge = true;
        }
      ];
    };
  };
}
