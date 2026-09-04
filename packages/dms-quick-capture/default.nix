{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation {
  pname = "dms-quick-capture";
  version = "5.3.0";

  src = pkgs.fetchFromGitHub {
    owner = "hthienloc";
    repo = "dms-quick-capture";
    rev = "v5.3.0";
    hash = "sha256-cBWDxkphkmaBKJXdocw7P3/+WYC/6FQG8PfDp8NL2v0=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp -r . $out/
  '';

  meta = {
    description = "DankMaterialShell plugin for screenshot annotation and screen recording";
    homepage = "https://github.com/hthienloc/dms-quick-capture";
    license = pkgs.lib.licenses.mit;
    platforms = pkgs.lib.platforms.all;
  };
}
