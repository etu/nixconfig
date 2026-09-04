{
  config,
  lib,
  osConfig,
  perSystem,
  pkgs,
  ...
}:
let
  # Compute the Gravatar hash at eval time from etu.user.email, so the
  # fetch itself stays a runtime action (no image baked into the store)
  # while still being driven declaratively per-host/per-user.
  gravatarHash = builtins.hashString "sha256" (lib.toLower osConfig.etu.user.email);
  gravatarUrl = "https://www.gravatar.com/avatar/${gravatarHash}?s=512&d=404";

  avatarSync = pkgs.writeShellApplication {
    name = "dms-avatar-sync";
    runtimeInputs = [
      pkgs.curl
      config.programs.dank-material-shell.package
    ];
    text = ''
      cache="$HOME/.cache/dms-avatar.png"
      tmp="$(mktemp)"
      trap 'rm -f "$tmp"' EXIT

      if ! curl -fsS --max-time 10 -o "$tmp" "${gravatarUrl}"; then
        echo "No gravatar set for ${osConfig.etu.user.email}, skipping" >&2
        exit 0
      fi

      mkdir -p "$(dirname "$cache")"
      mv "$tmp" "$cache"

      # dms.service being started doesn't guarantee its IPC handlers are
      # registered yet. Before the "profile" target exists, `dms ipc call`
      # prints "Target not found." but still exits 0, so a plain exit-code
      # check falsely treats that as success -- match on the actual
      # confirmation text instead.
      for _ in $(seq 1 15); do
        output="$(dms ipc call profile setImage "$cache" 2>&1 || true)"
        if printf '%s\n' "$output" | grep -q '^SUCCESS'; then
          exit 0
        fi
        sleep 2
      done

      echo "dms-avatar-sync: dms never became ready, giving up" >&2
      exit 1
    '';
  };
in
{
  # Configure DankMaterialShell, a Quickshell-based Material 3 desktop
  # shell (bar, launcher, notifications, etc).
  programs.dank-material-shell = {
    enable = true;

    systemd.enable = true;
    systemd.restartIfChanged = true;

    # System monitoring widgets are powered by dgop, which is built into
    # the dms daemon itself and needs no extra package.
    enableSystemMonitoring = true;

    # Plugins without a `settings` block aren't auto-detected as needing
    # plugin_settings.json managed, so their "enabled" state (separate
    # from just being installed) would otherwise depend on manually
    # toggling them on in Settings -> Plugins, and not be declarative.
    managePluginSettings = true;

    # Launcher plugin for searching/copying emoji and unicode characters,
    # triggered from the app launcher with ":e <query>".
    plugins.emojiLauncher.src = perSystem.self.dms-emoji-launcher;

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

      # Idle management, replaces swayidle's timeout-based lock/suspend.
      acLockTimeout = 300; # Lock after 5 minutes idle
      batteryLockTimeout = 300;
      acPostLockMonitorTimeout = 60; # Turn the screen off 1 minute after locking
      batteryPostLockMonitorTimeout = 60;
      acSuspendTimeout = if osConfig.etu.graphical.sway.enableSuspendOnTimeout then 600 else 0;
      batterySuspendTimeout = if osConfig.etu.graphical.sway.enableSuspendOnTimeout then 600 else 0;

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
            "music"
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

  # Sync the profile picture shown in dms-shell (avatar, lock screen)
  # from Gravatar, based on etu.user.email, via accounts-daemon.
  #
  # WantedBy=dms.service (not sway-session.target!) is deliberate: this
  # unit used to be WantedBy=sway-session.target with After=dms.service,
  # which formed an ordering cycle against dms.service's own
  # relationship to that target and deadlocked both units. Hanging
  # directly off dms.service instead gives a plain parent-child edge
  # with no path back through the target.
  systemd.user.services.dms-avatar-sync = {
    Unit = {
      Description = "Sync profile picture from Gravatar into dms-shell";
      After = [ "dms.service" ];
    };
    Service.Type = "oneshot";
    Service.ExecStart = lib.getExe avatarSync;
    Install.WantedBy = [ "dms.service" ];
  };
}
