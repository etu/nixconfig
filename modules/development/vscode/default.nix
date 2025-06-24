{
  config,
  lib,
  pkgs,
  ...
}: let
  vspkgs = {
    vscode-codeception = pkgs.callPackage ./extensions/vscode-codeception.nix {};
    vscode-ido = pkgs.callPackage ./extensions/vscode-ido.nix {};
    vscode-speech = pkgs.callPackage ./extensions/vscode-speech.nix {};
    volar = pkgs.callPackage ./extensions/volar.nix {};
    php-sniffer = pkgs.callPackage ./extensions/php-sniffer.nix {};
    openscad = pkgs.callPackage ./extensions/openscad.nix {};
    github-copilot-chat = pkgs.callPackage ./extensions/github-copilot-chat.nix {};
  };
in {
  options.etu.development.vscode.enable = lib.mkEnableOption "Enable development vscode settings";
  options.etu.development.vscode.enableWork = lib.mkEnableOption "Enable development vscode for work settings";

  config = lib.mkIf config.etu.development.vscode.enable {
    # Enable unfree packages on system level.
    etu.base.nix.allowUnfree = [
      "vscode-extension-bmewburn-vscode-intelephense-client"
      "vscode-extension-github-copilot"
    ];

    # Enable unfree packages on home level.
    etu.base.nix.allowUnfreeHome = [
      "vscode"
      "vscode-extension-bmewburn-vscode-intelephense-client"
    ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Enable vscode itself
      programs.vscode.enable = true;
      programs.vscode.mutableExtensionsDir = false;

      # It probably doesn't hurt to get notices about out of date extensions, especially when I
      # have extensions defined here in this file.
      programs.vscode.profiles.default.enableExtensionUpdateCheck = true;

      # TODO: disable vscode.php-language-features in code somehow
      programs.vscode.profiles.default.extensions =
        [
          pkgs.vscode-extensions.bbenoist.nix # .nix file extension
          pkgs.vscode-extensions.bmewburn.vscode-intelephense-client # Better php support
          pkgs.vscode-extensions.tuttieee.emacs-mcx # Emacs keybindings
          pkgs.vscode-extensions.kahole.magit # Magit emulation
          pkgs.vscode-extensions.golang.go # Go support
          pkgs.vscode-extensions.mkhl.direnv # Direnv
          pkgs.vscode-extensions.xdebug.php-debug # Php debug support
          vspkgs.vscode-ido # Decent file navigation
          vspkgs.php-sniffer # Php code style sniffing
          pkgs.vscode-extensions.github.copilot # Copilot
          vspkgs.github-copilot-chat # Copilot chat
        ]
        ++ (lib.optionals config.etu.graphical.fdm-printing.enable [
          vspkgs.openscad # Openscad support
        ])
        ++ (lib.optionals config.etu.development.vscode.enableWork [
          # Only on work computer
          vspkgs.vscode-codeception # Codeception support
          vspkgs.volar
        ]);

      # Enable vscode settings
      programs.vscode.profiles.default.userSettings =
        {
          "[html]"."editor.formatOnSave" = false;
          "[nix]"."editor.tabSize" = 2;
          "[php]"."editor.defaultFormatter" = "wongjn.php-sniffer";
          "editor.cursorBlinking" = "solid";
          "editor.cursorStyle" = "block";
          "editor.formatOnSave" = true;
          "editor.minimap.enabled" = false; # I find the minimap to be distracting and a waste of space
          "explorer.confirmDelete" = false;
          "files.insertFinalNewline" = true; # Make sure to have a final new line at end of files
          "files.trimFinalNewlines" = true; # Trim superfluous new lines at end of files
          "files.trimTrailingWhitespace" = true; # Trim whitespace at end of lines on save
          "github.copilot.enable"."markdown" = "true"; # Allow copilot to suggest in markdown files
          "phpSniffer.standard" = "PSR12";
          "telemetry.telemetryLevel" = "off";
          "workbench.editor.showTabs" = "none"; # I find tabs to be distracting
        }
        // (lib.optionalAttrs config.etu.development.vscode.enableWork {
          "[vue]"."editor.formatOnSave" = false;
        });

      # This configures the keyring used for copilot to remember logins across reboots
      home.file.".vscode/argv.json".text = builtins.toJSON {
        enable-crash-reporter = false;
        password-store = "gnome";
      };

      programs.vscode.profiles.default.keybindings = [
        {
          # Bind open folder
          key = "ctrl+x , p";
          command = "workbench.action.files.openFolder";
          when = "openFolderWorkspaceSupport";
        }
        {
          # Bind search for file within project to something useful
          key = "ctrl+x , f";
          command = "workbench.action.quickOpen";
        }
        {
          # Bind search for file within project to something useful
          key = "ctrl+x , a";
          command = "workbench.action.findInFiles";
        }
        {
          # Unbind default search keybind
          key = "ctrl+shift+f";
          command = "-workbench.action.findInFiles";
        }
        # No idea why I can't keybind this command to some other key, it just doesn't work.
        # {
        #   key = "ctrl+x ctrl+]";
        #   command = "editor.gotoNextSymbolFromResult";
        #   when = "hasSymbols";
        # }
        # {
        #   key = "f12";
        #   command = "-editor.gotoNextSymbolFromResult";
        #   when = "hasSymbols";
        # }
        #
        # The plan here was to simulate winner-undo and winner-redo
        # {
        #   key = "ctrl+c LeftArrow";
        #   command = "workbench.action.navigateBack";
        #   when = "canNavigateBack";
        # }
        # {
        #   key = "ctrl+c RightArrow";
        #   command = "workbench.action.navigateForward";
        #   when = "canNavigateForward";
        # }

        {
          # Bind ido to open file
          key = "ctrl+x ctrl+f";
          command = "extension.ido";
          when = "!terminalFocus";
        }
        {
          # Unbind default file open
          key = "ctrl+x ctrl+f";
          command = "-workbench.action.quickOpen";
        }

        # Magit keybinds
        {
          key = "ctrl+x g";
          command = "magit.status";
        }
        {
          # Unbind default keybind
          key = "alt+x g";
          command = "-magit.status";
        }
        {
          key = "n";
          command = "magit.move-next-entity";
          when = "editorTextFocus && editorLangId == 'magit'";
        }
        {
          key = "p";
          command = "magit.move-previous-entity";
          when = "editorTextFocus && editorLangId == 'magit'";
        }
      ];
    };

    # Enable persistence for vscode state files files.
    etu.base.zfs.user.directories = [
      ".config/Code/"
    ];
  };
}
