{
  config,
  flake,
  inputs,
  lib,
  pkgs,
  ...
}:
let
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

  # Function to build emacs with packages from use-package config
  buildEmacsPackage = configSubstituted:
    pkgs.emacsWithPackagesFromUsePackage {
      package = emacsPackage;
      
      # Don't assume ensuring of all use-package declarations
      alwaysEnsure = false;
      
      # config to be able to pull in use-package dependencies
      config = configSubstituted;
      
      # No extra packages needed, all handled via use-package
      extraEmacsPackages = _: [ ];
    };
in
{
  options.etu.base.emacs = {
    enable = lib.mkEnableOption "Enable base emacs settings";
    extraConfig = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "This allows to add strings that gets added to the emacs config file.";
    };
  };

  config = lib.mkIf config.etu.base.emacs.enable {
    # Import the emacs overlay from nix community to get the latest
    # and greatest packages.
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlay
    ];

    # Allow to install intelephense which is an unfree package.
    etu.base.nix.allowUnfree = [
      "intelephense"
      "copilot-language-server"
    ];

    # Allow unfree packages in home-manager as well
    etu.base.nix.allowUnfreeHome = [
      "intelephense"
      "copilot-language-server"
    ];

    # Install emacs icons symbols
    fonts.packages = [
      pkgs.emacs-all-the-icons-fonts
    ];

    # Configure emacs for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      imports = [
        flake.homeModules.emacs
      ];
      
      # Pass the package builder function and other needed values
      _module.args = {
        emacsPackageBuilder = buildEmacsPackage;
        emacsTreesitGrammars = treesitGrammars;
        emacsExtraPackages = extraPackages;
      };
    };

    # Configure emacs for root users home-manager.
    home-manager.users.root = {
      imports = [
        flake.homeModules.emacs
      ];
      
      # Pass the package builder function and other needed values
      _module.args = {
        emacsPackageBuilder = buildEmacsPackage;
        emacsTreesitGrammars = treesitGrammars;
        emacsExtraPackages = extraPackages;
      };
    };

    # Enable persistence for Emacs.
    etu.base.zfs.user.directories = [
      ".config/github-copilot"
      ".local/share/emacs"
    ];
  };
}
