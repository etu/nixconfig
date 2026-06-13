{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  pname = "jivetalking";
  version = "0.5.1";

  src = pkgs.fetchurl {
    url = "https://github.com/linuxmatters/jivetalking/releases/download/0.5.1/jivetalking-linux-amd64";
    hash = "sha256:1wv8bcl5h4wpv5fj2v3pirn62nlihlz0w4zr17jaa6yv23z7gsh8";
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
