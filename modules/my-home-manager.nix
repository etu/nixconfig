{ config, lib, pkgs, ... }:
let
  cfg = config.my.home-manager;

  swayEnabled = config.my.sway.enable;

  # Load sources
  sources = import ../nix/sources.nix;
in
{
  imports = [
    # Import the home-manager module
    "${sources.home-manager}/nixos"

    # Import home-manager configurations
    ./home-manager.d/emacs.nix
    ./home-manager.d/htop.nix
    ./home-manager.d/sway.nix
  ];

  config = lib.mkIf cfg.enable {
    # Make sure to start the home-manager activation before I log it.
    systemd.services."home-manager-${config.my.user.username}" = {
      before = [ "display-manager.service" ];
      wantedBy = [ "multi-user.target" ];
    };

    home-manager.users.${config.my.user.username} = {
        # Import a persistance module for home-manager.
        imports = [
          ./home-manager/weechat.nix
        ];

        programs.home-manager.enable = true;

        home.file = {
          # Home nix config.
          ".config/nixpkgs/config.nix".text = "{ allowUnfree = true; }";

          # Nano config
          ".nanorc".text = "set constantshow # Show linenumbers -c as default";

          # Tmux config
          ".tmux.conf".source = ./dotfiles/tmux.conf;

          # Fish config
          ".config/fish/config.fish".source = ./dotfiles/fish/config.fish;

          # Fish functions
          ".config/fish/functions".source = ./dotfiles/fish/functions;

          # Lorrirc
          ".direnvrc".text = ''
            use_nix() {
              eval "$(lorri direnv)"
            }
          '';

          # Some extra scripts
          "bin/git-branchclean".source = ./dotfiles/bin/git-branchclean;
          "bin/git-git".source = ./dotfiles/bin/git-git;
          "bin/git-lol".source = ./dotfiles/bin/git-lol;
          "bin/git-refetch-tags".source = ./dotfiles/bin/git-refetch-tags;
          "bin/restow".source = ./dotfiles/bin/restow;
          "bin/spacecolors".source = ./dotfiles/bin/spacecolors;

          "bin/keep".source = pkgs.runCommandNoCC "keep" { } ''
            cp ${./dotfiles/bin/keep} $out
            substituteInPlace $out --replace /bin/zsh ${pkgs.zsh}/bin/zsh
          '';
        } // lib.optionalAttrs swayEnabled {
          # Mpv config file - Don't show images embedded in music files
          ".config/mpv/mpv.conf".text = "no-audio-display";

          # .XCompose
          ".XCompose".text = ''
            include "%L"

            # Default already
            # <Multi_key> <a> <a>: "å"
            # <Multi_key> <A> <A>: "Å"

            # Some nice binds
            <Multi_key> <a> <e>: "ä"
            <Multi_key> <A> <E>: "Ä"
            <Multi_key> <o> <e>: "ö"
            <Multi_key> <O> <E>: "Ö"

            # Table flip multi key
            <Multi_key> <t> <f>: "(ノಠ益ಠ)ノ彡┻━┻"

            # Shruggie
            <Multi_key> <s> <h>: "¯\\_(ツ)_/¯"
          '';
        };

        programs.alacritty.enable = swayEnabled;
        programs.alacritty.settings = {
          env.TERM = "xterm-256color";
          font.size = config.my.fonts.size;
          font.normal.family = config.my.fonts.monospace;
          bell = {
            duration = 250;
            color = "#441111";
            animation = "EaseOut";
          };
          colors = {
            primary = { background = "#000000"; foreground = "#dddddd"; };
            normal = {
              black = "#000000";
              red = "#cc0403";
              green = "#19cb00";
              yellow = "#cecb00";
              blue = "#0d73cc";
              magenta = "#cb1ed1";
              cyan = "#0dcdcd";
              white = "#dddddd";
            };
            bright = {
              black = "#767676";
              red = "#f2201f";
              green = "#23fd00";
              yellow = "#fffd00";
              blue = "#1a8fff";
              magenta = "#fd28ff";
              cyan = "#14ffff";
              white = "#ffffff";
            };
          };
        };

        programs.git = {
          enable = true;

          # Default configs
          extraConfig = {
            commit.gpgSign = swayEnabled;

            user.name = config.my.user.realname;
            user.email = config.my.user.email;
            user.signingKey = config.my.user.signingKey;

            # Set default "git pull" behaviour so it doesn't try to default to
            # either "git fetch; git merge" (default) or "git fetch; git rebase".
            pull.ff = "only";
          };

          # Global ignores
          ignores = [ ".ac-php-conf.json" ];

          # Conditonally included configs
          includes = [{
            condition = "gitdir:/home/${config.my.user.username}/tvnu/";
            contents = {
              commit.gpgSign = false;
              user.email = config.my.user.workEmail;
              user.signingKey = "";
            };
          }];
        };

        programs.browserpass.enable = swayEnabled;

        xdg.mimeApps = {
          enable = swayEnabled;
          defaultApplications = {
            "text/html" = [ "firefox.desktop" ];
            "x-scheme-handler/http" = [ "firefox.desktop" ];
            "x-scheme-handler/https" = [ "firefox.desktop" ];
            "x-scheme-handler/about" = [ "firefox.desktop" ];
            "x-scheme-handler/unknown" = [ "firefox.desktop" ];
            "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
          };

          associations.added = {
            "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
          };
        };

        # GTK theme configs
        gtk.enable = swayEnabled;
        gtk.font.name = config.my.fonts.normal;
        gtk.font.size = builtins.floor config.my.fonts.size;
        gtk.gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;

        # Set up qt theme as well
        qt = {
          enable = swayEnabled;
          platformTheme = "gtk";
        };

        # Set the rofi font
        programs.rofi.font = "${config.my.fonts.monospace} ${toString (builtins.floor config.my.fonts.size)}";

        # Enable syncthing.
        services.syncthing.enable = swayEnabled;

        home.stateVersion = "20.09";
      };
  };
}
