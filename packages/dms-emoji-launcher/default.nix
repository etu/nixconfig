{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation {
  pname = "dms-emoji-launcher";
  version = "0-unstable-2026-07-04";

  src = pkgs.fetchFromGitHub {
    owner = "devnullvoid";
    repo = "dms-emoji-launcher";
    rev = "8ff394e3ddfcb2fd755ed2e7b4c6f01f3e26e596";
    hash = "sha256-fmIddCvACwO8wbAtLBMtDnEXXQJjb7+o2s4jW3f8VIU=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp -r . $out/
  '';

  meta = {
    description = "DankMaterialShell launcher plugin for searching and copying emoji and unicode characters";
    homepage = "https://github.com/devnullvoid/dms-emoji-launcher";
    license = pkgs.lib.licenses.mit;
    platforms = pkgs.lib.platforms.all;
  };
}
