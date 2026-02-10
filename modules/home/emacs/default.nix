{
  config,
  lib,
  osConfig,
  pkgs,
  flake,
  ...
}:
let
  # Apply the emacs overlay to get emacsWithPackagesFromUsePackage
  # Using pkgs.extend is more efficient than re-importing nixpkgs
  pkgsWithOverlay = pkgs.extend flake.inputs.emacs-overlay.overlay;

  # Get the emacs package with treesitter support
  emacsPackage = pkgsWithOverlay.emacs-pgtk;

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

  # Load the config file and substitute variables in pure Nix (avoiding IFD)
  emacsConfigRaw = builtins.readFile ./config.el;

  # Perform substitutions in pure Nix
  emacsConfigSubstituted = builtins.replaceStrings
    [
      "@treesitGrammars@"
      "@dataPrefix@"
      "@extraConfig@"
      "@fontname@"
      "@fontsize@"
    ]
    [
      "${treesitGrammars}/lib"
      osConfig.etu.dataPrefix
      (lib.concatStringsSep "\n\n" osConfig.etu.base.emacs.extraConfig)
      osConfig.etu.graphical.theme.fonts.monospace
      (toString osConfig.etu.graphical.theme.fonts.size)
    ]
    emacsConfigRaw;

  # Write the substituted config to a file for loading
  emacsConfigFile = pkgs.writeText "config.el" emacsConfigSubstituted;

  # Build emacs with packages from use-package config
  emacsWithPackages = pkgsWithOverlay.emacsWithPackagesFromUsePackage {
    package = emacsPackage;

    # Don't assume ensuring of all use-package declarations
    alwaysEnsure = false;

    # config to be able to pull in use-package dependencies
    config = emacsConfigSubstituted;

    # No extra packages needed, all handled via use-package
    extraEmacsPackages = _: [ ];
  };
in
{
  # Install language servers and tools as home packages
  home.packages = extraPackages;

  # Enable emacs in home-manager using programs.emacs
  programs.emacs = {
    enable = true;
    # Use the package built with overlay in this module
    package = emacsWithPackages;

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
        (load-file "${emacsConfigFile}"))
    '';
  };

  # Enable emacs service (daemon)
  services.emacs = {
    enable = true;
    client.enable = true;
  };
}
