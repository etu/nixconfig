function fish_prompt --description 'Display prompt'
    set last_ret $status

    if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    set PROMPT ''

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
    if test (prompt_pwd | sed -e 's/^~//' -e 's:/::g') != ''
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
    builtin echo -ns $PROMPT (set_color normal) ' '
end
