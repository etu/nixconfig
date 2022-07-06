{ config, lib, pkgs, ... }:
let
  cfg = config.my.common-cli;

in
{
  config = lib.mkIf cfg.enable {
    # Set your time zone.
    time.timeZone = "Europe/Stockholm";

    # Select internationalisation properties.
    i18n = {
      defaultLocale = "en_US.UTF-8";
      supportedLocales = [
        "all"
      ];
    };

    console.font = "Lat2-Terminus16";
    console.keyMap = "dvorak";

    # Enable the OpenSSH daemon.
    services.openssh.enable = true;
    services.openssh.passwordAuthentication = false;

    # Enable fish.
    programs.fish.enable = true;

    # Enable mosh.
    programs.mosh.enable = true;

    # Enable firewall.
    networking.firewall.enable = true;
    networking.firewall.allowPing = true;

    # Enable lorri on all systems.
    services.lorri.enable = true;

    # Enable doas on all systems.
    security.doas.enable = true;

    # Root shell
    users.extraUsers.root.shell = pkgs.fish;

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      bat
      bc # Dependency for some fish functions
      comma # The "," command which allows to run non-installed things ", htop"
      curl
      duf
      fd
      file
      fzf
      git
      host
      ncdu
      nix-top
      pv
      ripgrep
      tmux
      whois

      # Install prettyping
      prettyping

      # With pp shortcut
      (pkgs.runCommandNoCC "prettyping-pp" { } ''
        mkdir -p $out/bin
        ln -s ${pkgs.prettyping}/bin/prettyping $out/bin/pp
      '')

      # Install some color test scripts from xterm
      (pkgs.runCommandNoCC "xterm-color-scripts" { } ''
        tar -xf ${pkgs.xterm.src}

        install -Dm755 xterm-${pkgs.xterm.version}/vttests/256colors2.pl $out/bin/256colors2.pl
        install -Dm755 xterm-${pkgs.xterm.version}/vttests/88colors2.pl $out/bin/88colors2.pl
      '')
    ];

    home-manager.users.${config.etu.user.username} = lib.mkIf config.my.home-manager.enable {
      programs.fish.enable = true;
      programs.fish.shellInit = ''
        # Set editor
        set --global --export EDITOR emacs

        # Fix highligting of search-matches of background in less when we have TERM=screen
        set --global --export LESS_TERMCAP_se (echo -e '\e[0m')
        set --global --export LESS_TERMCAP_so (echo -e '\e[38;5;16m\e[48;5;15m')

        # Enable the direnv hook
        eval (direnv hook fish)
      '';
      programs.fish.loginShellInit = "set --erase fish_greeting";
      programs.fish.interactiveShellInit = ''
        # Put an ascii-fish as greeter if it's an interactive shell
        begin
          set -l PRIMARY (set_color red)
          set -l SECONDARY (set_color yellow)
          set --global fish_greeting $PRIMARY'                  ___
           ___======____='$SECONDARY'---='$PRIMARY')
          /T            \_'$SECONDARY'--==='$PRIMARY')
          L \ '$SECONDARY'(0)   '$PRIMARY'\~    \_'$SECONDARY'-=='$PRIMARY')
           \      / )J'$SECONDARY'~~    '$PRIMARY'\\'$SECONDARY'-='$PRIMARY')
            \\\\___/  )JJ'$SECONDARY'~~    '$PRIMARY'\)
             \_____/JJJ'$SECONDARY'~~      '$PRIMARY'\
             / \  , \\'$PRIMARY'J'$SECONDARY'~~~~      \
            (-\)'$PRIMARY'\='$SECONDARY'|  \~~~        L__
            ('$PRIMARY'\\'$SECONDARY'\\)  ( -\)_            ==__
             '$PRIMARY'\V    '$SECONDARY'\-'$PRIMARY'\) =='$SECONDARY'=_____  J\   \\\\
                    '$PRIMARY'\V)     \_)'$SECONDARY' \   JJ J\)
                                /J J'$PRIMARY'T'$SECONDARY'\JJJ'$PRIMARY'J)
                                (J'$SECONDARY'JJ| '$PRIMARY'\UUU)
                                 (UU)'
        end
      '';
      programs.fish.shellAbbrs = {
        "-" = "cd -";
        "nsh" = "nix-shell --run fish -p";
        "ipython" = "nix-shell --run ipython -p python3Packages.ipython";
      };
      programs.fish.functions = {
        "256colors" = ''
          for i in (seq 1 255)
            builtin echo -ne "\e[38;5;"$i"m"$i" "
          end

          builtin echo
        '';
        "bonk" = ''
          for arg in $argv
            set -l store_path (string unescape (nix-instantiate --eval --expr "with (import <nixpkgs> {}); builtins.toString (lib.getBin $arg)"))
            nix-store --quiet -r $store_path
            set PATH "$store_path/bin" $PATH
            set -g -a __bonk_pkgs $arg
          end
        '';
        "bonk-dump" = ''
          echo "{ pkgs ? import <nixpkgs> }:
          pkgs.mkShell {
            buildInputs = with pkgs; [ $__bonk_pkgs ];
          }
          " > shell.nix
        '';
        # Case insensitive find wrapper
        "isfind" = ''find . -iname "*"$argv[1]"*"'';
        # Print command duration in seconds for last command
        "ltime" = "echo (echo 'scale=3; ' $CMD_DURATION ' / 1000' | bc)'s'";
        # Adapted from https://github.com/rixx/dotfiles/blob/master/zsh/modules/colored-man.zsh
        # Which in turn was adatped from: https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/colored-man-pages/colored-man-pages.plugin.zsh
        "man" = ''
          env LESS_TERMCAP_mb=(printf "\e[1;35m") \
            LESS_TERMCAP_md=(printf "\e[1;35m") \
            LESS_TERMCAP_me=(printf "\e[0m") \
            LESS_TERMCAP_se=(printf "\e[0m") \
            LESS_TERMCAP_so=(printf "\e[1;34m") \
            LESS_TERMCAP_ue=(printf "\e[0m") \
            LESS_TERMCAP_us=(printf "\e[1;32m") man $argv
        '';
        "weather" = ''
          set -l location ""

          if count $argv >>/dev/null
            set location $argv[1]
          end

          curl "https://wttr.in/$location?lang=sv&M"
        '';
        # Render prompt
        "fish_prompt" = ''
          set last_ret $status

          if not set -q __fish_prompt_hostname
            set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
          end

          set PROMPT ""

          # Add User
          set PROMPT $PROMPT(set_color -b 585858)(set_color bbbbbb)' '$USER' '

          # Add hostname
          set PROMPT $PROMPT(set_color -b 444444)' '$__fish_prompt_hostname' '

          # Add CWD (home|root) with colors
          switch (prompt_pwd)
            case '~*' # If in home, add a nice colored ~
              set PROMPT $PROMPT(set_color -b 0087af)(set_color faf5e3)' ~ '

            case '*' # If not in home, probably in or somewhere below /, add a nice colored /
              set PROMPT $PROMPT(set_color -b afa700)(set_color faf5e3)' / '
          end

          # Add the rest of the CWD
          if test (prompt_pwd | sed -e 's/^~//' -e 's:/::g') != ""
            set PROMPT $PROMPT(set_color -b 3a3a3a)(set_color bbbbbb)(prompt_pwd | sed -e 's/^~//' -e 's:/: :g')' '
          end

          # Add colors depending on if previous command was successful or not
          if test $last_ret = 0
            set PROMPT $PROMPT(set_color -b 5faf00)(set_color faf5e3)
          else
            set PROMPT $PROMPT(set_color -b d7005f)(set_color faf5e3)
          end

          # Add sign at end of prompt depending on user
          if test (id -u) -eq 0
            set PROMPT $PROMPT' # '
          else
            set PROMPT $PROMPT' $ '
          end

          # Print prompt, also reset color and put an extra space there
          builtin echo -ns $PROMPT (set_color normal) " "
        '';
        # Render right prompt
        "fish_right_prompt" = ''
          set PROMPT (set_color -b 585858)(set_color bbbbbb)" "(date +%H:%M:%S)" "

          set git_prompt (__fish_git_prompt ' %s ')

          if test $status = 0
            set PROMPT (set_color -b 444444)(set_color bbbbbb)$git_prompt$PROMPT
          end

          if test -n "$IN_NIX_SHELL"
            if test "$IN_NIX_SHELL" = "pure"
              set PROMPT (set_color -b 0087af)(set_color faf5e3)" ❄ ️"$PROMPT
            else
              set PROMPT (set_color -b 897e01)(set_color faf5e3)" ❄ ️"$PROMPT
            end
          end

          builtin echo -ns $PROMPT(set_color normal)
        '';
      };
    };
  };
}
