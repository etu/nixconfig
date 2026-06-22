{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  pname = "jivetalking";
  version = "0.6.0";

  src = pkgs.fetchurl {
    url = "https://github.com/linuxmatters/jivetalking/releases/download/0.6.0/jivetalking-linux-amd64";
    hash = "sha256:1ijg89qmp88grishnyx2yp1mfgncnhklh5qczr6zpxdpngfm1yl4";
  };

  nativeBuildInputs = [ pkgs.autoPatchelfHook ];
  buildInputs = [ pkgs.stdenv.cc.cc.lib ];

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    install -Dm755 $src $out/bin/jivetalking
  '';

  meta = {
    description = "Raw microphone recordings into broadcast-ready audio in one command. No configuration, and no surprises 🕺";
    license = pkgs.lib.licenses.gpl3Only;
    platforms = [ "x86_64-linux" ];
  };
}
