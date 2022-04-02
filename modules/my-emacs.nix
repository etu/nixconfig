{ config, lib, pkgs, ... }:
let
  cfg = config.my.emacs;

  # Load sources
  sources = import ../nix/sources.nix;

  # Create a file with my config without path substitutes in place.
  myEmacsConfigPlain =
    pkgs.writeText "config-unsubstituted.el"
      (builtins.readFile ./emacs-files/base.el);

  # Run my config trough substituteAll to replace all paths with paths to
  # programs etc to have as my actual config file.
  myEmacsConfig = pkgs.runCommandNoCC "config.el" {
    fontname = config.my.fonts.monospace;
    fontsize = builtins.floor config.my.fonts.size;
  } "substituteAll ${myEmacsConfigPlain} $out";


  myEmacsLispLoader = extraLisp: loadFile: pkgs.writeText "${loadFile.name}-init.el"
    ''
      ;;; ${loadFile.name}-init.el -- starts here
      ;;; Commentary:
      ;;; Code:

      ;; Extra injected lisp.
      ${extraLisp}

      ;; Increase the threshold to reduce the amount of garbage collections made
      ;; during startups.
      (let ((gc-cons-threshold (* 50 1000 1000))
            (gc-cons-percentage 0.6)
            (file-name-handler-alist nil))

        ;; Load config
        (load-file "${loadFile}"))

      ;;; ${loadFile.name}-init.el ends here
    '';

  myEmacsInit =
    myEmacsLispLoader
      ''
        ;; Add a startup hook that logs the startup time to the messages buffer
        (add-hook 'emacs-startup-hook
            (lambda ()
                (message "Emacs ready in %s with %d garbage collections."
                    (format "%.2f seconds"
                        (float-time
                            (time-subtract after-init-time before-init-time)))
                        gcs-done)))
      ''
      myEmacsConfig;

  # Define language servers to include in the wrapper for Emacs
  extraBinPaths = [
    # Language Servers
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
    "${emacs.name}-with-extra-bin-paths"
    { nativeBuildInputs = [ pkgs.makeWrapper ]; }
    ''
      makeWrapper ${emacs}/bin/emacs $out/bin/${binName} --prefix PATH : ${lib.makeBinPath extraBinPaths}
    ''
  );

  myEmacsPackage = emacsPackage: pkgs.emacsWithPackagesFromUsePackage {
    package = emacsPackage;

    # Don't assume ensuring of all use-package declarations, this is
    # the default behaviour, but this gets rid of the notice.
    alwaysEnsure = false;

    # config to be able to pull in use-package dependencies from there.
    config = builtins.readFile myEmacsConfig;

    # Package overrides
    override = epkgs: epkgs // {
      # Add my config initializer as an emacs package
      myConfigInit = (
        pkgs.runCommandNoCC "my-emacs-default-package" { } ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${myEmacsInit} $out/share/emacs/site-lisp/default.el
        ''
      );

      cell-mode = let
        cellModeSrc = builtins.fetchurl {
          url = "https://gitlab.com/dto/mosaic-el/-/raw/f737583aab836cdf8891231d8ed6aa20df9377aa/cell.el";
          sha256 = "0clydvm1jq1wcjwms5465ngzyb76vwl9l9gcd1dxvv1898h03b9c";
        };
      in pkgs.runCommandNoCC "cell-mode" {} ''
        mkdir -p $out/share/emacs/site-lisp
        cp ${cellModeSrc} $out/share/emacs/site-lisp/cell.el
      '';

      # Override nix-mode source
      #nix-mode = epkgs.nix-mode.overrideAttrs (oldAttrs: {
      #  src = builtins.fetchTarball {
      #    url = https://github.com/nixos/nix-mode/archive/master.tar.gz;
      #  };
      #});
    };

    # Extra packages to install
    extraEmacsPackages = epkgs: (
      # Install my config file as a module
      [ epkgs.myConfigInit epkgs.cell-mode ] ++

      # Install work deps
      lib.optionals cfg.enableWork [
        epkgs.es-mode
        epkgs.jenkinsfile-mode
        epkgs.vcl-mode
      ]
    );
  };

  emacsPackages = {
    default = pkgs.emacs;
    nox = pkgs.emacs-nox;
    wayland = (import sources.emacs-overlay pkgs (pkgs // { inherit lib; })).emacsPgtkGcc;
  };
in
{
  config = lib.mkIf cfg.enable {
    # Import the emacs overlay from nix community to get the latest
    # and greatest packages.
    nixpkgs.overlays = lib.mkIf cfg.enable [
      (import sources.emacs-overlay)
    ];

    services.emacs = lib.mkIf cfg.enable {
      enable = true;
      defaultEditor = true;
      package = (wrapEmacsWithExtraBinPaths {
        emacs = (myEmacsPackage emacsPackages."${cfg.package}");
      });
    };


    # Install emacs icons symbols if we have any kind of graphical emacs
    fonts.fonts = lib.mkIf (cfg.enable && config.my.emacs.package != "nox") [
      pkgs.emacs-all-the-icons-fonts
    ];

    # Install aditional packages
    environment.systemPackages = (lib.optionals (config.my.emacs.package == "wayland") ([
      (wrapEmacsWithExtraBinPaths {
        emacs = (myEmacsPackage emacsPackages.default);
        binName = "emacs-x11";
      })
    ]));
  };
}
