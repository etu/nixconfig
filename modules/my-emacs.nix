{ config, lib, pkgs, ... }:
let
  cfg = config.my.home-manager;
  emacsCfg = config.my.emacs;

  # Load sources
  sources = import ../nix/sources.nix;

  # Run my config trough substituteAll to replace all paths with paths to
  # programs etc to have as my actual config file.
  emacsConfig = pkgs.runCommandNoCC "config.el" {
    fontname = config.my.fonts.monospace;
    fontsize = builtins.floor config.my.fonts.size;
  } "substituteAll ${./emacs-config.el} $out";

  # Function to wrap loading of a different emacs lisp file with
  # different garbage collection settings.
  emacsLispLoader = loadFile: pkgs.writeText "${loadFile.name}-init.el" ''
    ;;; ${loadFile.name}-init.el -- starts here
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
      (load-file "${loadFile}"))

    ;;; ${loadFile.name}-init.el ends here
  '';

  emacsConfigInit = emacsLispLoader emacsConfig;

  # Define language servers to include in the wrapper for Emacs
  extraBinPaths = [
    # Language Servers
    pkgs.go                                             # Go language
    pkgs.gopls                                          # Go language server
    pkgs.nodePackages.bash-language-server              # Bash language server
    pkgs.nodePackages.dockerfile-language-server-nodejs # Docker language server
    pkgs.nodePackages.intelephense                      # PHP language server
    pkgs.nodePackages.typescript-language-server        # JS/TS language server
    pkgs.nodePackages.vscode-css-languageserver-bin     # CSS/LESS/SASS language server
    pkgs.rnix-lsp                                       # Nix language server

    # Other programs
    pkgs.gnuplot                                        # For use with org mode
    pkgs.phpPackages.phpcs                              # PHP codestyle checker
  ];

  # Function to wrap emacs to contain the path for language servers
  wrapEmacsWithExtraBinPaths = (
    { emacs, binName ? "emacs" }: pkgs.runCommandNoCC
    "${emacs.name}-with-extra-bin-paths" { nativeBuildInputs = [ pkgs.makeWrapper ]; }
    ''
      makeWrapper ${emacs}/bin/emacs $out/bin/${binName} --prefix PATH : ${lib.makeBinPath extraBinPaths}
    ''
  );

  buildEmacsPackage = emacsPackage: pkgs.emacsWithPackagesFromUsePackage {
    package = emacsPackage;

    # Don't assume ensuring of all use-package declarations, this is
    # the default behaviour, but this gets rid of the notice.
    alwaysEnsure = false;

    # config to be able to pull in use-package dependencies from there.
    config = builtins.readFile emacsConfig;

    # Package overrides
    override = epkgs: epkgs // {
      # Add my config initializer as an emacs package
      myEmacsConfigInit = pkgs.runCommandNoCC "my-emacs-default-package" { } ''
        mkdir -p $out/share/emacs/site-lisp
        cp ${emacsConfigInit} $out/share/emacs/site-lisp/default.el
      '';
    };

    # Extra packages to install
    extraEmacsPackages = epkgs: (
      [ epkgs.myEmacsConfigInit ] ++

      # Install work deps
      lib.optionals emacsCfg.enableWork [
        epkgs.es-mode
        epkgs.jenkinsfile-mode
        epkgs.vcl-mode
      ]
    );
  };

  # Selection of emacs packages to choose from
  emacsPackages = {
    default = pkgs.emacs;
    nox = pkgs.emacs-nox;
    wayland = (import sources.emacs-overlay pkgs (pkgs // { inherit lib; })).emacsPgtkNativeComp;
  };
in
{
  config = lib.mkIf (cfg.enable && emacsCfg.enable) {
    # Import the emacs overlay from nix community to get the latest
    # and greatest packages.
    nixpkgs.overlays = [
      (import sources.emacs-overlay)
    ];

    # Install my emacs package system-wide.
    services.emacs = {
      enable = true;
      defaultEditor = true;
      package = (wrapEmacsWithExtraBinPaths {
        emacs = buildEmacsPackage emacsPackages.${emacsCfg.package};
      });
    };

    # Write emacs configs to the home directory
    home-manager.users.${config.my.user.username}.home.file.".emacs".text = "(setq-default inhibit-startup-screen t)";

    # Install emacs icons symbols if we have any kind of graphical emacs
    fonts.fonts = lib.mkIf (emacsCfg.package != "nox") [
      pkgs.emacs-all-the-icons-fonts
    ];

    environment.systemPackages = (lib.optionals (config.my.emacs.package == "wayland") ([
      (wrapEmacsWithExtraBinPaths {
        emacs = buildEmacsPackage emacsPackages.default;
        binName = "emacs-x11";
      })
    ]));
  }; # END config
}
