function fish_right_prompt --description 'Display right prompt'
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
end
