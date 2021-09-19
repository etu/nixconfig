{ pkgs ? import <nixpkgs> { }, ... }:

let
  langserv = pkgs.callPackage ./vscode-intelephense {};
in
pkgs.runCommandNoCC "init-vscode-intelephense" { } ''
  mkdir -p $out/bin

  ln -s ${langserv.package}/lib/node_modules/init-intelephense/node_modules/intelephense/lib/intelephense.js $out/bin/intelephense
''
