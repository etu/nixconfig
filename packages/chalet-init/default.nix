{pkgs, ...}: let
  chaletPkg = (pkgs.callPackage ./chalet {}).package;
in
  pkgs.runCommand "chalet-${chaletPkg.version}" {} ''
    mkdir -p $out/bin

    ln -s ${chaletPkg}/lib/node_modules/init-chalet/node_modules/chalet/lib/cli/bin.js $out/bin/chalet
  ''
