{ config, lib, pkgs, ... }:
let
  cfg = config.my.home-manager;

  swayEnabled = config.my.sway.enable;

  # Load sources
  sources = import ../nix/sources.nix;
in
{
  imports = [
    # Import home-manager configurations
    ./home-manager.d/htop.nix
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

        home.stateVersion = "20.09";
      };
  };
}
