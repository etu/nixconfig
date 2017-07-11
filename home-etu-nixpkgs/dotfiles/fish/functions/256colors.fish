function 256colors
    for i in (seq 1 255)
        builtin echo -ne "\e[38;5;"$i"m"$i" "
    end

    builtin echo
end
