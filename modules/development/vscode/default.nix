{
  config,
  lib,
  pkgs,
  ...
}: let
  vspkgs = {
    vscode-org-mode = pkgs.vscode-utils.extensionFromVscodeMarketplace {
      # This is super barebones, but at least it got highligts for headlines:
      # https://marketplace.visualstudio.com/items?itemName=vscode-org-mode.org-mode
      publisher = "vscode-org-mode";
      name = "org-mode";
      version = "1.0.0";
      sha256 = "sha256-o9CIjMlYQQVRdtTlOp9BAVjqrfFIhhdvzlyhlcOv5rY=";
    };
    vscode-ido = pkgs.vscode-utils.extensionFromVscodeMarketplace {
      # This actually gives me a quite good file selection experience:
      # https://marketplace.visualstudio.com/items?itemName=kimgronqvist.vscode-ido
      publisher = "kimgronqvist";
      name = "vscode-ido";
      version = "0.3.0";
      sha256 = "sha256-Pr3o7FkVu7r0V+PGcs5BRqBh3iXcXOwPRCb2MtoAZJ4=";
    };
    volar = pkgs.vscode-utils.extensionFromVscodeMarketplace {
      # https://marketplace.visualstudio.com/items?itemName=Vue.volar
      publisher = "Vue";
      name = "volar";
      version = "1.8.27";
      sha256 = "sha256-6FktlAJmOD3dQNn2TV83ROw41NXZ/MgquB0RFQqwwW0=";
    };
    php-sniffer = pkgs.vscode-utils.extensionFromVscodeMarketplace {
      # https://marketplace.visualstudio.com/items?itemName=wongjn.php-sniffer
      publisher = "wongjn";
      name = "php-sniffer";
      version = "1.3.0";
      sha256 = "sha256-dPF1CRX9WVQFyC7RZxiPDtIg6+oUituY0qEn5Hipd5Q=";
    };
    php-debug = pkgs.vscode-utils.extensionFromVscodeMarketplace {
      # https://marketplace.visualstudio.com/items?itemName=xdebug.php-debug
      publisher = "xdebug";
      name = "php-debug";
      version = "1.34.0";
      sha256 = "sha256-WAcXWCMmvuw7nkfGcOgmK+s+Nw6XpvNR4POXD85E/So=";
    };
  };
in {
  options.etu.development.vscode.enable = lib.mkEnableOption "Enable development vscode settings";
  options.etu.development.vscode.enableWork = lib.mkEnableOption "Enable development vscode for work settings";

  config = lib.mkIf config.etu.development.vscode.enable {
    # Not sure why this is needed on both system level and in home manager.
    etu.base.nix.allowUnfree = [
      "vscode-extension-bmewburn-vscode-intelephense-client"
      "vscode-extension-github-copilot"
      "vscode-extension-github-copilot-chat"
    ];

    # If my user exists, enable home-manager configurations
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      nixpkgs.config.allowUnfreePredicate = pkg:
        builtins.elem (lib.getName pkg) [
          "vscode"
          "vscode-extension-bmewburn-vscode-intelephense-client"
        ];

      # Enable vscode itself
      programs.vscode.enable = true;
      programs.vscode.mutableExtensionsDir = false;

      # It probably doesn't hurt to get notices about out of date extensions, especially when I
      # have extensions defined here in this file.
      programs.vscode.enableExtensionUpdateCheck = true;

      # TODO: disable vscode.php-language-features in code somehow
      programs.vscode.extensions =
        [
          pkgs.vscode-extensions.bbenoist.nix # .nix file extension
          pkgs.vscode-extensions.bmewburn.vscode-intelephense-client # Better php support
          pkgs.vscode-extensions.tuttieee.emacs-mcx # Emacs keybindings
          pkgs.vscode-extensions.kahole.magit # Magit emulation
          vspkgs.vscode-org-mode # Really basic org-mode highlighting
          vspkgs.vscode-ido # Decent file navigation
          vspkgs.php-sniffer # Php code style sniffing
          vspkgs.php-debug # Php debug support
          pkgs.vscode-extensions.github.copilot # Copilot
          pkgs.vscode-extensions.github.copilot-chat # Copilot chat
        ]
        ++ (lib.optionals config.etu.development.vscode.enableWork [
          # Only on work computer
          vspkgs.volar
        ]);

      # Enable vscode settings
      programs.vscode.userSettings = {
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
      };

      # This configures the keyring used for copilot to remember logins across reboots
      home.file.".vscode/argv.json".text = builtins.toJSON {
        enable-crash-reporter = false;
        password-store = "gnome";
      };

      programs.vscode.keybindings = [
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
