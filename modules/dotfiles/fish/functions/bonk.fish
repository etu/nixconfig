function bonk
    for arg in $argv
        set -l store_path (string unescape (nix-instantiate --eval --expr "with (import <nixpkgs> {}); builtins.toString (lib.getBin $arg)"))
        nix-store --quiet -r $store_path
        set PATH "$store_path/bin" $PATH
        set -g -a __bonk_pkgs $arg
    end
end

function bonk-dump
    echo "{ pkgs ? import <nixpkgs> }:
pkgs.mkShell {
  buildInputs = with pkgs; [ $__bonk_pkgs ];
}
" > shell.nix
end
