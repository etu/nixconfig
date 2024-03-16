{
  config,
  emacs-overlay,
  emacsWayland,
  intelephense,
  lib,
  pkgs,
  ...
}: let
  # Run my config trough substituteAll to replace font names from my
  # system font settings.
  emacsConfig = pkgs.runCommand "config.el" {
    inherit treesitGrammars;
    inherit (config.etu) dataPrefix;
    extraConfig = lib.concatStringsSep "\n\n" config.etu.base.emacs.extraConfig;
    fontname = config.etu.graphical.theme.fonts.monospace;
    fontsize = config.etu.graphical.theme.fonts.size;
  } "substituteAll ${./config.el} $out";

  # Config to wrap loading of the emacs config file.
  emacsConfigInit = pkgs.writeText "${emacsConfig.name}-init.el" ''
    ;;; ${emacsConfig.name}-init.el -- starts here
    ;;; Commentary:
    ;;; Code:

    ;; Add a startup hook that logs the startup time to the messages buffer
    (add-hook 'emacs-startup-hook
        (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                (format "%.2f seconds"
                    (float-time
                        (time-subtract after-init-time before-init-time)))
                    gcs-done)))

    ;; Increase the threshold to reduce the amount of garbage collections made
    ;; during startups.
    (let ((gc-cons-threshold (* 50 1000 1000))
          (gc-cons-percentage 0.6)
          (file-name-handler-alist nil))

      ;; Load config
      (load-file "${emacsConfig}"))

    ;;; ${emacsConfig.name}-init.el ends here
  '';

  # Define language servers to include in the wrapper for Emacs
  extraBinPaths = [
    # Language Servers
    pkgs.go # Go language
    pkgs.gopls # Go language server
    pkgs.nodePackages.bash-language-server # Bash language server
    pkgs.nodePackages.dockerfile-language-server-nodejs # Docker language server
    intelephense # PHP language server
    pkgs.nodePackages.typescript-language-server # JS/TS language server
    pkgs.nodePackages.vscode-css-languageserver-bin # CSS/LESS/SASS language server
    pkgs.nodejs # For copilot.el

    # Other programs
    pkgs.gnuplot # For use with org mode
    pkgs.phpPackages.php-codesniffer # PHP codestyle checker
    pkgs.openscad # For use with scad and scad preview mode
  ];

  # List custom treesitter grammars
  treesitGrammars = emacsPackages.${config.etu.base.emacs.package}.pkgs.treesit-grammars.with-grammars (g:
    with g; [
      tree-sitter-bash
      tree-sitter-c
      tree-sitter-cmake
      tree-sitter-cpp
      tree-sitter-css
      tree-sitter-dockerfile
      tree-sitter-go
      tree-sitter-gomod
      tree-sitter-hcl
      tree-sitter-html
      tree-sitter-java
      tree-sitter-json
      tree-sitter-latex
      tree-sitter-make
      tree-sitter-nix
      tree-sitter-php
      tree-sitter-python
      tree-sitter-rust
      tree-sitter-sql
      tree-sitter-toml
      tree-sitter-yaml
    ]);

  # Function to wrap emacs to contain the path for language servers
  wrapEmacsWithExtraBinPaths = {
    emacs ? emacsPackages.${config.etu.base.emacs.package},
    extraWrapperArgs ? "",
  }:
    pkgs.runCommand "${emacs.name}-with-extra-bin-paths" {nativeBuildInputs = [pkgs.makeWrapper];}
    ''
      makeWrapper ${buildEmacsPackage emacs}/bin/emacs $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath extraBinPaths} ${extraWrapperArgs}
    '';

  buildEmacsPackage = emacsPackage:
    pkgs.emacsWithPackagesFromUsePackage {
      package = emacsPackage;

      # Don't assume ensuring of all use-package declarations, this is
      # the default behaviour, but this gets rid of the notice.
      alwaysEnsure = false;

      # config to be able to pull in use-package dependencies from there.
      config = builtins.readFile emacsConfig;

      # Extra packages to install
      extraEmacsPackages = epkgs: [
        # Add my config initializer as an emacs package
        (pkgs.runCommand "my-emacs-default-package" {} ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${emacsConfigInit} $out/share/emacs/site-lisp/default.el
        '')

        # Install copilot.el
        (epkgs.trivialBuild {
          pname = "copilot";
          version = "2023-09-13";

          packageRequires = with epkgs; [dash editorconfig s];

          preInstall = ''
            mkdir -p $out/share/emacs/site-lisp
            cp -vr $src/dist $out/share/emacs/site-lisp
          '';

          src = pkgs.fetchFromGitHub {
            owner = "zerolfx";
            repo = "copilot.el";
            rev = "421703f5dd5218ec2a3aa23ddf09d5f13e5014c2";
            hash = "sha256-7LKB2JoYAVu5i23ZKBVZPe6azCItHK/tlBNPgFXxztc=";
          };
        })
      ];
    };

  # Selection of emacs packages to choose from
  emacsPackages = {
    default = pkgs.emacs;
    nox = pkgs.emacs-nox;
    wayland = emacsWayland;
  };
in {
  options.etu.base.emacs = {
    enable = lib.mkEnableOption "Enable base emacs settings";
    extraConfig = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "This allows to add strings that gets added to the emacs config file.";
    };
    package = lib.mkOption {
      type = lib.types.str;
      default = "default";
      defaultText = "default";
      description = "Which emacs package to use.";
    };
  };

  config = lib.mkIf config.etu.base.emacs.enable {
    # Import the emacs overlay from nix community to get the latest
    # and greatest packages.
    nixpkgs.overlays = [
      emacs-overlay
    ];

    # Allow to install intelephense which is an unfree package.
    etu.base.nix.allowUnfree = ["intelephense"];

    # Install my emacs package system-wide.
    services.emacs = {
      enable = true;
      defaultEditor = true;
      package = wrapEmacsWithExtraBinPaths {};
    };

    # Install emacs icons symbols if we have any kind of graphical emacs
    fonts.packages = lib.mkIf (config.etu.base.emacs.package != "nox") [
      pkgs.emacs-all-the-icons-fonts
    ];

    # Configure emacs for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.file.".emacs".text = "(setq-default inhibit-startup-screen t)";
    };

    # Configure emacs for root users home-manager.
    home-manager.users.root.home.file.".emacs".text = "(setq-default inhibit-startup-screen t)";

    # Enable persistence for Emacs.
    etu.base.zfs.user.directories = [
      ".config/github-copilot"
      ".local/share/emacs"
    ];
  };
}
