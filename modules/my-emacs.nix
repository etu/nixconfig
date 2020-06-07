{ config, lib, pkgs, ... }:

let
  cfg = config.my.emacs;

  emacs-overlay = (import ../nix/sources.nix).emacs-overlay;

  # Extract the path executed by the systemd-service, to get the flags used by
  # the service. But replace the path with the security wrapper dir so we get
  # the suid enabled path to the binary.
  physlockCommand =
    builtins.replaceStrings
      [ (pkgs.physlock + "/bin") ]
      [ config.security.wrapperDir ]
      config.systemd.services.physlock.serviceConfig.ExecStart;


  # Create a file with my config without path substitutes in place.
  myEmacsConfigPlain =
    pkgs.writeText "config-unsubstituted.el"
      (builtins.readFile ./emacs-files/base.el);

  # Create a file with my exwm config without path substitutes in place.
  myExwmConfigPlain =
    pkgs.writeText "exwm-config-unsubstituted.el"
      (builtins.readFile ./emacs-files/exwm.el);


  # Run my config trough substituteAll to replace all paths with paths to
  # programs etc to have as my actual config file.
  myEmacsConfig = (
    pkgs.runCommand "config.el"
      (with pkgs; {
        inherit gnuplot gocode;
        phpcs = phpPackages.phpcs;
        phpcbf = phpPackages.phpcbf;
      }) "substituteAll ${myEmacsConfigPlain} $out"
  );

  # Run my exwm config through substituteAll to replace all paths with paths
  # to programs etc to have as my actual config file.
  myExwmConfig = (
    pkgs.runCommand "exwm-config.el"
      (with pkgs; {
        inherit systemd kitty flameshot;
        lockCommand = physlockCommand;
        xbacklight = acpilight;
        rofi = rofi.override { plugins = [ pkgs.rofi-emoji ]; };
      }) "substituteAll ${myExwmConfigPlain} $out"
  );

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

  myExwmInit = myEmacsLispLoader "" myExwmConfig;

in
{
  options.my.emacs = {
    enable = lib.mkEnableOption "Enables emacs with the modules I want";
    enableExwm = lib.mkEnableOption "Enables EXWM config and graphical environment";
    enableWork = lib.mkEnableOption "Enables install of work related modules";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.emacs;
      defaultText = "pkgs.emacs";
      description = "Which emacs package to use";
    };
  };

  config = lib.mkIf cfg.enable {
    # Import the emacs overlay from nix community to get the latest
    # and greatest packages.
    nixpkgs.overlays = lib.mkIf cfg.enable [
      (import emacs-overlay)
    ];

    services.emacs = lib.mkIf cfg.enable {
      enable = true;
      defaultEditor = true;

      package = (pkgs.emacsWithPackagesFromUsePackage {
        package = cfg.package;

        # Config to parse, use my built config from above
        config = builtins.readFile myEmacsConfig;

        # Package overrides
        override = epkgs: epkgs // {
          # Add my config initializer as an emacs package
          myConfigInit = (
            pkgs.runCommand "my-emacs-default-package"
              { } ''
              mkdir -p  $out/share/emacs/site-lisp
              cp ${myEmacsInit} $out/share/emacs/site-lisp/default.el
            ''
          );

          # Override nix-mode source
          nix-mode = epkgs.nix-mode.overrideAttrs (oldAttrs: {
            src = builtins.fetchTarball {
              url = https://github.com/nixos/nix-mode/archive/master.tar.gz;
            };
          });
        };

        # Extra packages to install
        extraEmacsPackages = epkgs: (
          # Install my config file as a module
          [ epkgs.myConfigInit ] ++

          # Install exwm deps
          lib.optionals cfg.enableExwm [ epkgs.exwm epkgs.desktop-environment ] ++

          # Install work deps
          lib.optionals cfg.enableWork [
            epkgs.es-mode
            epkgs.vcl-mode
            epkgs.vue-mode
            epkgs.vue-html-mode
          ]
        );
      });
    };


    fonts.fonts = lib.mkIf cfg.enable (with pkgs; [
      emacs-all-the-icons-fonts
    ]);


    # Libinput
    services.xserver = lib.mkIf cfg.enableExwm {
      libinput.enable = true;

      # Loginmanager
      displayManager.lightdm.enable = true;
      displayManager.lightdm.autoLogin.enable = true;
      displayManager.lightdm.autoLogin.user = config.my.user.username;

      # Needed for autologin
      displayManager.defaultSession = "none+exwm";

      # Set up the login session
      windowManager.session = lib.singleton {
        name = "exwm";
        start = ''
          # Keybind:                           ScrollLock -> Compose,      <> -> Compose
          ${pkgs.xorg.xmodmap}/bin/xmodmap -e 'keycode 78 = Multi_key' -e 'keycode 94 = Multi_key'
          ${config.services.emacs.package}/bin/emacs -l ${myExwmInit}
        '';
      };

      # Enable auto locking of the screen
      xautolock.enable = true;
      xautolock.locker = physlockCommand;
      xautolock.enableNotifier = true;
      xautolock.notify = 15;
      xautolock.notifier = "${pkgs.libnotify}/bin/notify-send \"Locking in 15 seconds\"";
      xautolock.time = 3;
    };

    # Enable physlock and make a suid wrapper for it
    services.physlock.enable = cfg.enableExwm;
    services.physlock.allowAnyUser = cfg.enableExwm;

    # Enable autorandr for screen setups.
    services.autorandr.enable = cfg.enableExwm;

    # Set up services needed for gnome stuff for evolution
    services.gnome3.evolution-data-server.enable = cfg.enableExwm;
    services.gnome3.gnome-keyring.enable = cfg.enableExwm;

    # Install aditional packages
    environment.systemPackages = lib.mkIf cfg.enableExwm (with pkgs; [
      evince
      gnome3.adwaita-icon-theme # Icons for gnome packages that sometimes use them but don't depend on them
      gnome3.evolution
      scrot
      pavucontrol
    ]);
  };
}
