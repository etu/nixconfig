{ config, lib, pkgs, ... }:
let
  cfg = config.my.home-manager;

  swayEnabled = config.my.sway.enable;

  # Load sources
  sources = import ../nix/sources.nix;
in
{
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

        programs.htop = {
          enable = true;
          settings = {
            hide_userland_threads = true;
            hide_kernel_threads = true;
            highlight_base_name = true;
            shadow_other_users = true;
            show_program_path = true;
            tree_view = true;

            left_meters = [ "LeftCPUs" "Memory" "Swap" "ZFSARC" "ZFSCARC" ];
            left_meter_modes = [ 1 1 1 2 2 ];

            right_meters = [ "RightCPUs" "Tasks" "LoadAverage" "Uptime" "Battery" ];
            right_meter_modes = [ 1 2 2 2 2 ];
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

        home.stateVersion = "20.09";
      };
  };
}
