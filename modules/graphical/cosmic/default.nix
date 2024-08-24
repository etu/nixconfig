#
# Experimental Cosmic desktop module.
#
# This is a module with my settings for the System76 Cosmic desktop[1]
# on NixOS using some helper modules[2].
#
# [1]: https://system76.com/cosmic
# [2]: https://github.com/lilyinstarlight/nixos-cosmic
#
# Todo/Issues:
# - Keymap doesn't work on initial login screen.
# - Set a different background image.
#
{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.graphical.cosmic.enable = lib.mkEnableOption "Enable graphical cosmic settings";

  config = lib.mkIf config.etu.graphical.cosmic.enable {
    # Enable binary cache for cosmic builds
    nix.settings = {
      substituters = ["https://cosmic.cachix.org/"];
      trusted-public-keys = ["cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="];
    };

    # Enable cosmic desktop
    services.desktopManager.cosmic.enable = true;
    services.displayManager.cosmic-greeter.enable = true;

    # Force disable TLP in favor of something else from the cosmic modules.
    services.tlp.enable = lib.mkForce false;

    # Force a different pinentry since the gnome3 one doesn't seem to
    # work well at the moment, executing the gnome3 one on cosmic
    # gives me the following message:
    # "No Gcr System Prompter available, falling back to curses"
    programs.gnupg.agent.pinentryPackage = lib.mkForce pkgs.pinentry-qt;

    # Enable a wayland build of Emacs.
    etu.base.emacs.package = "wayland";

    # Set environment variable to make Electron and Chromium
    # applications run under wayland.
    # environment.sessionVariables.NIXOS_OZONE_WL = "1";

    # Install some programs
    etu.user.extraUserPackages = [
      pkgs.bluetuith
      pkgs.evince
      pkgs.imv
      pkgs.pavucontrol
      pkgs.wdisplays
      pkgs.wlr-randr
      pkgs.wofi
      pkgs.wofi-emoji
    ];

    # Make sure to start the home-manager activation before I log in.
    systemd.services."home-manager-${config.etu.user.username}" = {
      before = ["display-manager.service"];
      wantedBy = ["multi-user.target"];
    };

    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.file = {
        ".config/cosmic/com.system76.CosmicAppletTime/v1/military_time".text = "true";
        ".config/cosmic/com.system76.CosmicAppletTime/v1/first_day_of_week".text = "0";
        ".config/cosmic/com.system76.CosmicPanel.Panel/v1/opacity".text = "0.8";
        ".config/cosmic/com.system76.CosmicPanel.Panel/v1/plugins_center".text = ''
          Some([
            "com.system76.CosmicAppletWorkspaces",
          ])
        '';
        ".config/cosmic/com.system76.CosmicPanel.Panel/v1/plugins_wings".text = ''
          Some(([
            "com.system76.CosmicPanelWorkspacesButton",
            "com.system76.CosmicPanelAppButton",
          ], [
            "com.system76.CosmicAppletTime",
            "com.system76.CosmicAppletInputSources",
            "com.system76.CosmicAppletStatusArea",
            "com.system76.CosmicAppletTiling",
            "com.system76.CosmicAppletAudio",
            "com.system76.CosmicAppletNetwork",
            "com.system76.CosmicAppletBattery",
            "com.system76.CosmicAppletNotifications",
            "com.system76.CosmicAppletBluetooth",
            "com.system76.CosmicAppletPower",
          ]))
        '';
        ".config/cosmic/com.system76.CosmicPanel.Dock/v1/autohide".text = ''
          Some((
            wait_time: 1000,
            transition_time: 0,
            handle_size: 4,
          ))
        '';
        ".config/cosmic/com.system76.CosmicPanel.Dock/v1/size".text = "L";
        ".config/cosmic/com.system76.CosmicPanel.Dock/v1/opacity".text = "0.8";
        ".config/cosmic/com.system76.CosmicComp/v1/autotile".text = "true";
        ".config/cosmic/com.system76.CosmicComp/v1/autotile_behavior".text = "PerWorkspace";
        ".config/cosmic/com.system76.CosmicComp/v1/workspaces".text = ''
          (
            workspace_mode: OutputBound,
            workspace_layout: Horizontal,
          )
        '';
        ".config/cosmic/com.system76.CosmicBackground/v1/all".text = ''
          (
            output: "all",
            source: Color(Single((0.0, 0.286, 0.427))),
            filter_by_theme: false,
            rotation_frequency: 900,
            filter_method: Lanczos,
            scaling_mode: Zoom,
            sampling_method: Alphanumeric,
          )
        '';
        ".config/cosmic/com.system76.CosmicSettings.Shortcuts/v1/custom".text = ''
          {
            (
              modifiers: [
                Super,
              ],
              key: "p",
            ): Focus(Up),
            (
              modifiers: [
                Super,
                Shift,
              ],
              key: "j",
            ): Disable,
            (
              modifiers: [
                Super,
              ],
              key: "l",
            ): Disable,
            (
              modifiers: [
                Super,
              ],
              key: "Return",
            ): System(Terminal),
            (
              modifiers: [
                Super,
                Shift,
              ],
              key: "p",
            ): Move(Up),
            (
              modifiers: [
                Super,
                Shift,
              ],
              key: "b",
            ): Move(Left),
            (
              modifiers: [
                Super,
              ],
              key: "h",
            ): Disable,
            (
              modifiers: [
                Super,
                Shift,
              ],
              key: "l",
            ): Disable,
            (
              modifiers: [
                Super,
              ],
              key: "k",
            ): Disable,
            (
              modifiers: [
                Super,
              ],
              key: "f",
            ): Focus(Right),
            (
              modifiers: [
                Super,
                Shift,
              ],
              key: "f",
            ): Move(Right),
            (
              modifiers: [
                Super,
                Shift,
              ],
              key: "h",
            ): Disable,
            (
              modifiers: [
                Super,
              ],
              key: "j",
            ): Disable,
            (
              modifiers: [
                Super,
              ],
              key: "b",
            ): Focus(Left),
            (
              modifiers: [
                Super,
                Shift,
              ],
              key: "k",
            ): Disable,
            (
              modifiers: [
                Super,
                Shift,
              ],
              key: "n",
            ): Move(Down),
            (
              modifiers: [
                Super,
              ],
              key: "n",
            ): Focus(Down),
          }
        '';
      }; # END home.file
    };
  };
}
