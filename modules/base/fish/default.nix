{ config, lib, pkgs, ... }:

let
  base = {
    # Enable fish in home-manager
    programs.fish.enable = true;
    programs.fish.shellInit = ''
      # Fix highligting of search-matches of background in less when we have TERM=screen
      set --global --export LESS_TERMCAP_se (builtin echo -e '\e[0m')
      set --global --export LESS_TERMCAP_so (builtin echo -e '\e[38;5;16m\e[48;5;15m')

      # Enable the direnv hook
      if command -v direnv > /dev/null
        eval (direnv hook fish)
      end
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
      "nrun" = "nix run nixpkgs#";
      "ipython" = "nix run nixpkgs#python3Packages.ipython";
    };
    programs.fish.functions = {
      "256colors" = ''
        for i in (seq 1 255)
          builtin echo -ne "\e[38;5;"$i"m"$i" "
        end

        builtin echo
      '';
      # Case insensitive find wrapper
      "isfind" = ''find . -iname "*"$argv[1]"*"'';
      # Print command duration in seconds for last command
      "ltime" = "builtin echo (builtin echo 'scale=3; ' $CMD_DURATION ' / 1000' | ${pkgs.bc}/bin/bc)'s'";
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

        ${pkgs.curl}/bin/curl "https://wttr.in/$location?lang=sv&M"
      '';
      # Render prompt
      "fish_prompt" = ''
        set last_ret $status

        set PROMPT ""

        # Add user depending on if you're admin or not
        switch $USER
          case '${config.etu.user.username}'
            set PROMPT $PROMPT(set_color -b 585858)(set_color bbbbbb)' 👤 '

          case 'root' # Make a red background on root user to make it stand out.
            set PROMPT $PROMPT(set_color -b d7005f)(set_color faf5e3)' 👤 '

          case '*' # Also print username if not my own user or root.
            set PROMPT $PROMPT(set_color -b 585858)(set_color bbbbbb)' 👤 '$USER' '
        end

        # Set a variable to remember "who am i" output length to reduce the amount of subshells during usage.
        if not set -q __fish_prompt_who_am_i_count
          set -g __fish_prompt_who_am_i_count (who am i | wc -l)
        end

        # Add hostname if it's a remote connection
        if test $__fish_prompt_who_am_i_count = 0
          set PROMPT $PROMPT(set_color -b 444444)(set_color bbbbbb)' 💻 '
        else
          set PROMPT $PROMPT(set_color -b 444444)(set_color bbbbbb)' 🔗 '$hostname' '
        end

        # Add CWD (home|root) with colors
        switch (prompt_pwd)
          case '~*' # If in home, add a background block and a house
            set PROMPT $PROMPT(set_color -b 0087af)(set_color faf5e3)' 🏠 '

          case '*' # If not in home, probably in or somewhere below /, add a different colored block and a folder icon
            set PROMPT $PROMPT(set_color -b f1c40f)(set_color 000000)' 📁 '
        end

        # Add the rest of the CWD
        if test (prompt_pwd | sed -e 's/^~//' -e 's:/::g') != ""
          set PROMPT $PROMPT(set_color -b 3a3a3a)(set_color bbbbbb)(prompt_pwd | sed -e 's/^~//' -e 's:/: :g')' '
        end

        # Add colors and symbols depending on if previous command was successful or not
        if test $last_ret = 0
          set PROMPT $PROMPT(set_color -b 5faf00)(set_color faf5e3)' 👍 '
        else
          set PROMPT $PROMPT(set_color -b d7005f)(set_color faf5e3)' 👎 '
        end

        # Print prompt, also reset color and put an extra space there
        builtin echo -ns $PROMPT (set_color normal)' '
      '';
      # Render right prompt
      "fish_right_prompt" = ''
        set PROMPT ""

        # Add nix shell indicatior
        if test -n "$IN_NIX_SHELL"
          set PROMPT $PROMPT(set_color -b 0087af)(set_color faf5e3)' ❄ '
        end

        # Add git branch
        set git_prompt (__fish_git_prompt '%s ')
        if test $status = 0
          set PROMPT $PROMPT(set_color -b 444444)(set_color bbbbbb)' 🔀 '$git_prompt
        end

        # Add time
        set PROMPT $PROMPT(set_color -b 585858)(set_color bbbbbb)' '(date +%H:%M:%S)' '

        builtin echo -ns $PROMPT(set_color normal)
      '';
    }; # END programs.fish.functions
  };
in
{
  options.etu.base.fish.enable = lib.mkEnableOption "Enable base fish settings";

  config = lib.mkIf config.etu.base.fish.enable {
    # Enable fish.
    programs.fish.enable = true;

    # My main users shell.
    users.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      shell = pkgs.fish;
    };

    # Root shell.
    users.users.root.shell = pkgs.fish;

    # Configure fish for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable base;

    # Configure fish for root users home-manager.
    home-manager.users.root = base;

    # Enable persistence for fish files.
    etu.base.zfs.user.directories = [
      ".config/fish"
      ".local/share/fish"
    ];
  };
}
