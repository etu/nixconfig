function ltime --description 'Print command duration in seconds for last command'
    echo (echo 'scale=3; ' $CMD_DURATION ' / 1000' | bc)"s"
end
