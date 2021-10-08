{ pkgs ? import <nixpkgs> { }, ... }:

let
  intelephensePkg = (pkgs.callPackage ./vscode-intelephense { }).package;
  intelephense = pkgs.runCommandNoCC "intelephense-package" { } ''
    mkdir -p $out/bin

    ln -s ${intelephensePkg}/lib/node_modules/init-intelephense/node_modules/intelephense/lib/intelephense.js $out/bin/intelephense
  '';
in {
  inherit intelephense;
}
