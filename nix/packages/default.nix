{ pkgs ? import <nixpkgs> { }, lib ? pkgs.lib, ... }:

let
  intelephenseSources = (pkgs.callPackage ./vscode-intelephense { });
  intelephensePkg = intelephenseSources.package;

  # Extract version of the intelephense dependency used to have in the
  # nix store path of the script.
  intelephenseVersion = (lib.head (
    builtins.attrValues (
      lib.filterAttrs (n: v: v.packageName == "intelephense") intelephenseSources.sources
    )
  )).version;

  # Build a wropper derivation.
  intelephense = pkgs.runCommandNoCC "intelephense-package-${intelephenseVersion}" { } ''
    mkdir -p $out/bin

    ln -s ${intelephensePkg}/lib/node_modules/init-intelephense/node_modules/intelephense/lib/intelephense.js $out/bin/intelephense
  '';
in {
  inherit intelephense;
}
