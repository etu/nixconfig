###
# Set editor
##
set --global --export EDITOR emacs

###
# Fix highligting of search-matches of background in less when we have TERM=screen
##
set --global --export LESS_TERMCAP_se (echo -e '\e[0m')
set --global --export LESS_TERMCAP_so (echo -e '\e[38;5;16m\e[48;5;15m')


###
# Put an ascii-fish as greeter if it's an interactive shell, otherwise erase the greeter message
##
if status --is-interactive
    set --global fish_greeting (set_color red)'                  ___
   ___======____='(set_color yellow)'---='(set_color red)')
  /T            \_'(set_color yellow)'--==='(set_color red)')
  L \ '(set_color yellow)'(0)   '(set_color red)'\~    \_'(set_color yellow)'-=='(set_color red)')
   \      / )J'(set_color yellow)'~~    '(set_color red)'\\'(set_color yellow)'-='(set_color red)')
    \\\\___/  )JJ'(set_color yellow)'~~    '(set_color red)'\)
     \_____/JJJ'(set_color yellow)'~~      '(set_color red)'\
     / \  , \\'(set_color red)'J'(set_color yellow)'~~~~      \
    (-\)'(set_color red)'\='(set_color yellow)'|  \~~~        L__
    ('(set_color red)'\\'(set_color yellow)'\\)  ( -\)_            ==__
     '(set_color red)'\V    '(set_color yellow)'\-'(set_color red)'\) =='(set_color yellow)'=_____  J\   \\\\
            '(set_color red)'\V)     \_)'(set_color yellow)' \   JJ J\)
                        /J J'(set_color red)'T'(set_color yellow)'\JJJ'(set_color red)'J)
                        (J'(set_color yellow)'JJ| '(set_color red)'\UUU)
                         (UU)'
else
    set --erase fish_greeting
end
