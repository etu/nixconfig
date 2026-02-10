{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:
let
  # Get the emacs package with treesitter support
  emacsPackage = pkgs.emacs-pgtk;

  # List custom treesitter grammars
  treesitGrammars = emacsPackage.pkgs.treesit-grammars.with-grammars (
    g: with g; [
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
    ]
  );

  # Define language servers and tools to include in PATH for Emacs
  extraPackages = [
    # Language Servers
    pkgs.go # Go language
    pkgs.gopls # Go language server
    pkgs.bash-language-server # Bash language server
    pkgs.dockerfile-language-server # Docker language server
    pkgs.intelephense # PHP language server
    pkgs.nodePackages.typescript-language-server # JS/TS language server
    pkgs.vscode-langservers-extracted # CSS/LESS/SASS language server
    pkgs.nodejs # For copilot.el

    # Other programs
    pkgs.gnuplot # For use with org mode
    pkgs.phpPackages.php-codesniffer # PHP codestyle checker
    pkgs.openscad # For use with scad and scad preview mode
  ];

  # Load the config file and substitute variables
  emacsConfig = pkgs.runCommand "config.el" {
    inherit treesitGrammars;
    dataPrefix = osConfig.etu.dataPrefix;
    extraConfig = lib.concatStringsSep "\n\n" osConfig.etu.base.emacs.extraConfig;
    fontname = osConfig.etu.graphical.theme.fonts.monospace;
    fontsize = osConfig.etu.graphical.theme.fonts.size;
  } "substituteAll ${../../../modules/nixos/base/emacs/config.el} $out";
in
{
  # Install language servers and tools as home packages
  home.packages = extraPackages;

  # Enable emacs in home-manager using programs.emacs
  programs.emacs = {
    enable = true;
    package = (
      pkgs.emacsWithPackagesFromUsePackage {
        package = emacsPackage;

        # Don't assume ensuring of all use-package declarations
        alwaysEnsure = false;

        # config to be able to pull in use-package dependencies
        config = builtins.readFile emacsConfig;

        # No extra packages needed, all handled via use-package
        extraEmacsPackages = _: [ ];
      }
    );

    # Initialize with the config file
    extraConfig = ''
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
    '';
  };

  # Enable emacs service (daemon)
  services.emacs = {
    enable = true;
    client.enable = true;
  };
}
