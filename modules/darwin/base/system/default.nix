_: {
  # Use dark mode.
  system.defaults.NSGlobalDomain.AppleInterfaceStyle = "Dark";

  # Hide the Dock until the pointer reaches the screen edge.
  system.defaults.dock.autohide = true;

  # Don't show recently used apps in the Dock.
  system.defaults.dock.show-recents = false;

  # Show the battery percentage in the menu bar.
  system.defaults.controlcenter.BatteryShowPercentage = true;

  # Show the Bluetooth control in the menu bar.
  system.defaults.controlcenter.Bluetooth = true;

  # Faster key repeat (in units of 15 ms): start repeating after 375 ms (one
  # step below the shortest delay in System Settings), then repeat every 30 ms
  # (the fastest rate in System Settings).
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 25;
  system.defaults.NSGlobalDomain.KeyRepeat = 2;

  # Remap Caps Lock to Control.
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Use Touch ID for sudo.
  security.pam.services.sudo_local.touchIdAuth = true;
}
