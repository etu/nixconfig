function weather
    set -l location ""

    if count $argv >>/dev/null
        set location $argv[1]
    end

    curl "http://wttr.in/$location?lang=sv&M"
end
