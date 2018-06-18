use_nix() {
    local shell_file=$(test -f shell.nix && echo shell.nix || echo default.nix)
    local cache_key=$(nix-instantiate "$shell_file" 2> /dev/null | shasum -a 1 | cut -d ' ' -f 1)

    # Use ram as virtualenv storage
    local tmpdir
    case $(uname -s) in
        Linux*) tmpdir=$XDG_RUNTIME_DIR;;
        Darwin*) tmpdir_SDK=$TMPDIR;;
        *) tmpdir=/tmp
    esac
    if test "$tmpdir" = ""; then
        tmpdir="/tmp"
    fi

    # Set up cachedir
    local cachedir="${tmpdir}/direnv-nix"
    mkdir -p $cachedir

    local cache="$cachedir/$cache_key"
    if [[ ! -e "$cache" ]] || \
           [[ "$HOME/.direnvrc" -nt "$cache" ]] || \
           [[ ".envrc" -nt "$cache" ]] || \
           [[ "default.nix" -nt "$cache" ]] || \
           [[ "shell.nix" -nt "$cache" ]];
    then
        local tmp="$(mktemp "${cache_key}.tmp-XXXXXXXX")"
        trap "rm -rf '$tmp'" EXIT
        nix-instantiate ./shell.nix --indirect --add-root ~/.direnv-gcroots/"$cache_key"
        nix-shell --show-trace "$@" --run 'direnv dump' > "$tmp" && \
            mv "$tmp" "$cache"
    fi
    direnv_load cat "$cache"
    if [[ $# = 0 ]]; then
        watch_file default.nix
        watch_file shell.nix
    fi
}
