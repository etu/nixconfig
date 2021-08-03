# Adapted from https://github.com/rixx/dotfiles/blob/master/zsh/modules/colored-man.zsh
# Which in turn was adatped from:
# https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/colored-man-pages/colored-man-pages.plugin.zsh
function man
    env LESS_TERMCAP_mb=(printf "\e[1;35m") \
        LESS_TERMCAP_md=(printf "\e[1;35m") \
        LESS_TERMCAP_me=(printf "\e[0m") \
        LESS_TERMCAP_se=(printf "\e[0m") \
        LESS_TERMCAP_so=(printf "\e[1;34m") \
        LESS_TERMCAP_ue=(printf "\e[0m") \
        LESS_TERMCAP_us=(printf "\e[1;32m") man $argv
end
