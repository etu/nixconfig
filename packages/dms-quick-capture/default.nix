{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation {
  pname = "dms-quick-capture";
  version = "5.4.0";

  src = pkgs.fetchFromGitHub {
    owner = "hthienloc";
    repo = "dms-quick-capture";
    rev = "v5.4.0";
    hash = "sha256-uV5jCVwYkoOKdQwX/NXt62fZZd6OMcvD+94ySbkLArQ=";
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
