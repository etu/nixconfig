{ pkgs ? import <nixpkgs> { }, ... }:

let
  chaletPkg = (pkgs.callPackage ./chalet { }).package;
  chalet = pkgs.runCommand "chalet-package" { } ''
    mkdir -p $out/bin

    ln -s ${chaletPkg}/lib/node_modules/init-chalet/node_modules/chalet/lib/cli/bin.js $out/bin/chalet
  '';
in {
  inherit chalet;
}
