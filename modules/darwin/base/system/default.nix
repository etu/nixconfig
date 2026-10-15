_: {
  # Use dark mode.
  system.defaults.NSGlobalDomain.AppleInterfaceStyle = "Dark";

  # Hide the Dock until the pointer reaches the screen edge.
  system.defaults.dock.autohide = true;

  # Show the Bluetooth control in the menu bar.
  system.defaults.controlcenter.Bluetooth = true;

  # Remap Caps Lock to Control.
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Use Touch ID for sudo.
  security.pam.services.sudo_local.touchIdAuth = true;
}
