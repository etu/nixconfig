function d
    if test -d $argv
        ls -alh $argv
    else if test -f $argv
        bat $argv
    else
        echo "$argv: File or directory not found."
    end
end
